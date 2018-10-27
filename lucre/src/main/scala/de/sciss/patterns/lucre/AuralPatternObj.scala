/*
 *  AuralPatternObj.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.{LongPoint2D, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, DummySerializerFactory, IdentifierMap, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.patterns
import de.sciss.patterns.lucre.AuralPatternObj.ElemHandle
import de.sciss.patterns.{Event, Pat}
import de.sciss.serial.Serializer
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.proc.impl.AuralScheduledBase
import de.sciss.synth.proc.impl.AuralTimelineBase.spanToPoint
import de.sciss.synth.proc.{AuralContext, AuralObj, TimeRef}

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.concurrent.stm.{Ref, TSet}

object AuralPatternObj extends AuralObj.Factory {
  def tpe: Obj.Type = Pattern

  type Repr[~ <: stm.Sys[~]] = Pattern[~]

  private[this] lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def apply[S <: Sys[S]](pat: Pattern[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](pat)(tx, system, context) // IntelliJ highlight bug
    res.init(pat)
  }

  private type Leaf[S <: Sys[S]] = (SpanLike, Vec[(stm.Source[S#Tx, S#Id], AuralObj[S])])

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](value: Pattern[S])
                                                     (implicit tx: S#Tx, system: S { type I = I1 },
                                                      context: AuralContext[S]): AuralPatternObj[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx _
    implicit val itx: I1#Tx = iSys(tx)
    implicit val pointView: (Leaf[S], I1#Tx) => LongPoint2D = (l, _) => spanToPoint(l._1)
    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, Leaf[S]] =
      DummySerializerFactory[I1].dummySerializer

    val tree = SkipOctree.empty[I1, LongSpace.TwoDim, Leaf[S]](BiGroup.MaxSquare)

    new AuralPatternObj[S, I1](tx.newHandle(value), tree /* , viewMap */)
  }

  protected final case class ElemHandle[S <: Sys[S], Elem](idH: stm.Source[S#Tx, S#Id], span: SpanLike, view: Elem)
}
final class AuralPatternObj[S <: Sys[S], I1 <: stm.Sys[I1]](val objH: stm.Source[S#Tx, Pattern[S]],
                                                            tree: SkipOctree[I1, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#Id], AuralObj[S])])]
                                                           )
                                                           (implicit protected val context: AuralContext[S],
                                                            system: S { type I = I1 },
                                                            protected val iSys: S#Tx => I1#Tx)
  extends AuralScheduledBase[S, Unit, AuralObj[S]]
    with AuralObj[S] {
  attr =>

  import TxnLike.peer

  def tpe: Obj.Type = Pattern

  type ViewId     = Unit
  type Elem       = AuralObj[S]
  type ElemHandle = AuralPatternObj.ElemHandle[S, Elem]
  type Target     = Unit
  type Model      = Elem // AuralAttribute.Scalar

  private[this] val playingRef = TSet.empty[ElemHandle]

  private[this] var viewMap   : IdentifierMap[S#Id, S#Tx, ElemHandle] = _

  private[this] var patObserver: Disposable[S#Tx] = _

  private type Ctx  = patterns.lucre.Context[S, I1]
  private type St   = patterns.Stream[I1, Any]

  private[this] val patContext  = Ref.make[Ctx]
  private[this] val streamRef   = Ref(Option.empty[St])
  private[this] val streamPos   = Ref(Long.MaxValue)

  private type Leaf = (SpanLike, Vec[(stm.Source[S#Tx, S#Id], Elem)])

  @inline
  private def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)

  private def setPattern(g: Pat[_])(implicit tx: S#Tx): Unit = {
    implicit val itx: I1#Tx = iSys(tx)
    playingRef.foreach(elemRemoved(_, elemPlays = true))(tx.peer)
    tree.clear()
    disposeStream()
  }

  private def disposeStream()(implicit tx: S#Tx): Unit = {
    implicit val itx: I1#Tx = iSys(tx)
    streamRef.swap(None)(tx.peer).foreach(_.dispose())
  }

  def init(pat: Pattern[S])(implicit tx: S#Tx): this.type = {
    viewMap = tx.newInMemoryIdMap[ElemHandle]
    val graph0 = pat.value
    setPattern(graph0)
    patObserver = pat.changed.react { implicit tx => upd =>
      setPattern(upd.now)
    }
    this
  }

  /** Called during preparation of armed elements. This
    * happens either during initial `prepare` or during grid-events.
    * Given the `prepareSpan`, the sub-class should
    *
    * - find the elements using an `intersect`
    * - for each build a view and store it somewhere
    * - for each view call `prepareChild`
    * - accumulate the results of `prepareChild` into a `Map` that is returned.
    *
    * The map will become part of `IPreparing`.
    *
    * @param initial  if `true` this is an initial preparation which means the method
    *                 must include views that start before `spanP` if their span
    *                 overlaps with `spanP`. If `false` this is a follow up from
    *                 `gridReached` and the search must be restricted to views that
    *                 start no earlier than `spanP`.
    */
  protected def processPrepare(spanP: Span, timeRef: TimeRef, initial: Boolean)
                              (implicit tx: S#Tx): Iterator[PrepareResult] =
    new AbstractIterator[PrepareResult] {
      private[this] val patObj: Pattern[S]  = objH()
      private[this] var time  : Long        = streamPos.get(tx.peer)

      implicit private[this] val itx: I1#Tx = iSys(tx)
      implicit private[this] val ctx: Ctx   = patContext.get(tx.peer)

      private[this] val st: St = streamRef.get(tx.peer) match {
        case Some(_st) if !initial && time == spanP.start => _st
        case _ =>
          implicit val _ctx: Ctx = patterns.lucre.Context.dual[S](patObj)
          patContext.update(_ctx)(tx.peer)
          val _st: St = _ctx.expandDual(patObj.value) // g.expand[I1]
          streamRef.swap(Some(_st))(tx.peer).foreach(_.dispose())
          _st
      }

      private[this] var patSpan: Span = _
      private[this] var patView: AuralObj[S] = _

      private[this] var _hasNext  = true
      private[this] var countLoop = 0     // cheesy way to avoid infinite loops

      def hasNext: Boolean = _hasNext

      @tailrec
      private def advance(): Unit = {
        countLoop += 1
        if (countLoop < 100 && time < spanP.stop && ctx.hasNext(st)) {
          ctx.next(st) match {
            case evt: Event =>
              val delta = Event.delta(evt)
              if (delta > 0.0) {
                val legato      = Event.legato(evt)
                val sustain     = delta * legato
                val deltaFrames = (TimeRef.SampleRate * delta).toLong
                val durFrames   = math.max(32L, (TimeRef.SampleRate * sustain).toLong)    // XXX TODO hard-coded min
                patSpan         = Span(time, time + durFrames)
                time += deltaFrames

                evt.get(Event.keyPlay) match {
                  case Some(key: String) =>
                    patObj.attr.get(key) match {
                      case Some(playObj) =>
                        val hit = if (initial) patSpan.overlaps(spanP) else patSpan.start >= spanP.start
                        if (hit) {
                          patView = AuralObj(playObj)
                        } else {
                          countLoop = 0
                          advance()
                        }

                      case _ => advance()
                    }

                  case _ => advance()
                }
              } else {
                advance()
              }

            case _ => advance()
          }

        } else {
          _hasNext = false
          if (countLoop >= 100) Console.err.println("Pattern does not advance (100 elements counted)")
        }
      }

      def next(): (Target, SpanLike, Elem) = {
        if (!_hasNext) Iterator.empty.next()
        val res = ((), patSpan, patView)
        countLoop = 0
        advance()
        res
      }

      // constructor
      {
        advance()
        streamPos.set(spanP.stop)(tx.peer)
      }
    }

  /** Report the next interesting frame greater than the given frame for which
    * `eventReached` (internal) and `processEvent` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected def viewEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    BiGroupImpl.eventAfter(tree)(offset)(iSys(tx)).getOrElse(Long.MaxValue)

  /** Report the next interesting frame greater than the given frame for which
    * `gridReached` (internal) and `processPrepare` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected def modelEventAfter(offset: Long)(implicit tx: S#Tx): Long = {
    math.max(0L, offset + 1)
//    val offsetC = math.max(-1L, offset)
//    tree.floor(offsetC + 1)(iSys(tx)) match {
//      case Some((time0, pred0)) =>
//        if (time0 > offset) time0   // i.e. time0 == offset + 1
//        else {
//          val existing = viewEventAfter(offset)
//          if (existing != Long.MaxValue) existing else {
//            @tailrec
//            def loop(pred: Elem): Long =
//              nextElemFromStream(??? /* pred.stop */) match {
//                case Some(succ) =>
//                  if (??? /* succ.start >= offset */) ??? // succ.start
//                  else loop(succ)
//                case None => Long.MaxValue
//              }
//
//            loop(??? /* pred0 */)
//          }
//        }
//      case None =>
//        if (??? /* isEmptyRef() */) Long.MaxValue
//        else {    // we lost the cache
//          val graph = objH().value
//          // println("RESETTING GRAPH [0]")
//          if (??? /* !setPattern(graph) */) Long.MaxValue // became empty
//          else modelEventAfter(offset)  // repeat
//        }
//    }
  }

  @inline
  private[this] def intersect(offset: Long)(implicit tx: S#Tx): Iterator[Leaf] =
    BiGroupImpl.intersectTime(tree)(offset)(iSys(tx))

  protected def processPlay(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    val toStart = intersect(timeRef.offset)
    playViews(toStart, timeRef, target)
  }

//  protected def processPlay(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
//    implicit val itx: I1#Tx = iSys(tx)
//    tree.floor(timeRef.offset).foreach { case (_, entry) =>
//      playEntry(entry, timeRef = timeRef, target = target)
//    }
//  }

  private def playEntry(entry: Elem, timeRef: TimeRef, target: Target)
                       (implicit tx: S#Tx): Unit = {
    val childTime = timeRef.child(??? /* entry.span */)
    playView(??? /* entry */, childTime, target)
  }

  protected def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    val start = timeRef.offset
    val entry = tree.get(??? /* start */)(iSys(tx))
      .getOrElse(throw new IllegalStateException(s"No element at event ${timeRef.offset}"))
    ??? // playEntry(entry, timeRef = timeRef, target = play.target)
  }

  protected def elemFromHandle(h: ElemHandle): Elem = h.view

  protected def mkView(vid: Unit, span: SpanLike, obj: Model)(implicit tx: S#Tx): ElemHandle = ??? // obj

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: S#Tx): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      ??? // h.start > currentOffset && h.start == oldTarget
    }

  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: Target)
                        (implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    // logA(s"grapheme - playView: $view - $timeRef")
    view.run(timeRef, target)
    playingRef.add(h)
    viewPlaying(h)
  }

  private def playViews(it: Iterator[Leaf], timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit =
    if (it.hasNext) it.foreach { case (span, views) =>
      val tr = timeRef.child(span)
      views.foreach { case (idH, elem) =>
        playView(ElemHandle(idH, span, elem), tr, target)
      }
    }

  /** A notification method that may be used to `fire` an event
    * such as `AuralObj.Timeline.ViewAdded`.
    */
  protected def viewPlaying(h: ElemHandle)(implicit tx: S#Tx): Unit = ???

  /** A notification method that may be used to `fire` an event
    * such as `AuralObj.Timeline.ViewRemoved`.
    */
  protected def viewStopped(h: ElemHandle)(implicit tx: S#Tx): Unit = ???

  protected def stopView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    // logA(s"aural - stopView: $view")
    view.stop()
    view.dispose()
    playingRef.remove(h)
    removeView(h)
  }

  protected def stopViews()(implicit tx: S#Tx): Unit = {
    playingRef.foreach { h =>
      stopView(h)
    }
    disposeStream()
  }

  private def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    import h._
//    logA(s"timeline - removeView - $span - $view")

    // note: this doesn't have to check for `IPreparing`, as it is called only
    // via `eventReached`, thus during playing. correct?

    // preparingViews.remove(view).foreach(_.dispose())
    tree.transformAt(spanToPoint(span)) { opt =>
      opt.flatMap { case (span1, views) =>
        val i = views.indexWhere(_._2 == view)
        val views1 = if (i >= 0) {
          views.patch(i, Nil, 1)
        } else {
          Console.err.println(s"Warning: pattern - removeView - view for $objH not in tree")
          views
        }
        if (views1.isEmpty) None else Some(span1 -> views1)
      }
    } (iSys(tx))

    viewMap.remove(idH())
  }

//  private def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
//    implicit val itx: I1#Tx = iSys(tx)
//    val start = ??? // h.start
//    tree.remove(start)
//    // println(s"removeView($h)")
//  }
}
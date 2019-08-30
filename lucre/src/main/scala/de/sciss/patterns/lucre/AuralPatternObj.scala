/*
 *  AuralPatternObj.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre

import de.sciss.equal.Implicits._
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.{LongPoint2D, LongRectangle, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, DummySerializerFactory, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.patterns
import de.sciss.patterns.lucre.AuralPatternObj.ElemHandle
import de.sciss.patterns.lucre.impl.AuralProcEvtImpl
import de.sciss.patterns.{Event, Pat}
import de.sciss.serial.Serializer
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.proc.impl.AuralScheduledBase
import de.sciss.synth.proc.impl.AuralTimelineBase.spanToPoint
import de.sciss.synth.proc.{AuralContext, AuralObj, Proc, Runner, TimeRef, logAural => logA}

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.concurrent.stm.{Ref, TSet}

object AuralPatternObj extends AuralObj.Factory {
  def tpe: Obj.Type = Pattern

  type Repr[~ <: stm.Sys[~]] = Pattern[~]

  private[this] lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def apply[S <: Sys[S]](pat: Pattern[S], attr: Runner.Attr[S]
                        )(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val system  = tx.system
    // XXX TODO --- pass on `attr`
    val res     = prepare[S, system.I](pat)(tx, system, context) // IntelliJ highlight bug
    res.init(pat)
  }

//  private type Leaf[S <: Sys[S]] = (SpanLike, Vec[(stm.Source[S#Tx, S#Id], AuralObj[S])])
  private type Leaf[S <: Sys[S]] = (SpanLike, Vec[AuralObj[S]])

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](value: Pattern[S])
                                                     (implicit tx: S#Tx, system: S { type I = I1 },
                                                      context: AuralContext[S]): AuralPatternObj[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx
    implicit val itx: I1#Tx = iSys(tx)
    implicit val pointView: (Leaf[S], I1#Tx) => LongPoint2D = (l, _) => spanToPoint(l._1)
    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, Leaf[S]] =
      DummySerializerFactory[I1].dummySerializer

    val tree = SkipOctree.empty[I1, LongSpace.TwoDim, Leaf[S]](BiGroup.MaxSquare)

    new AuralPatternObj[S, I1](tx.newHandle(value), tree /* , viewMap */)
  }

  protected final case class ElemHandle[S <: Sys[S], Elem](/* idH: stm.Source[S#Tx, S#Id], */ span: SpanLike, view: Elem)
}
final class AuralPatternObj[S <: Sys[S], I1 <: stm.Sys[I1]](objH: stm.Source[S#Tx, Pattern[S]],
//                                                            tree: SkipOctree[I1, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#Id], AuralObj[S])])]
                                                            tree: SkipOctree[I1, LongSpace.TwoDim, (SpanLike, Vec[AuralObj[S]])]
                                                           )
                                                           (implicit protected val context: AuralContext[S],
                                                            system: S { type I = I1 },
                                                            protected val iSys: S#Tx => I1#Tx)
  extends AuralScheduledBase[S, Unit, AuralObj[S]]
    with AuralObj[S] {
  attr =>

  import TxnLike.peer

  def tpe: Obj.Type = Pattern

  type Repr = Pattern[S]

  def obj(implicit tx: S#Tx): Pattern[S] = objH()

  type ViewId     = Unit
  type Elem       = AuralObj[S]
  type ElemHandle = AuralPatternObj.ElemHandle[S, Elem]
  type Target     = Unit

//  type Model      = Elem // AuralAttribute.Scalar
  type Model      = (Event, Obj[S])

  private[this] val playingRef = TSet.empty[ElemHandle]

//  private[this] var viewMap   : IdentifierMap[S#Id, S#Tx, ElemHandle] = _

  private[this] var patObserver: Disposable[S#Tx] = _

  private type Ctx  = patterns.lucre.Context[S, I1]
  private type St   = patterns.Stream[I1, Any]

  private[this] val patContext  = Ref.make[Ctx]
  private[this] val streamRef   = Ref(Option.empty[St])
  private[this] val streamPos   = Ref(0L)

//  private type Leaf = (SpanLike, Vec[(stm.Source[S#Tx, S#Id], Elem)])
  private type Leaf = (SpanLike, Vec[Elem])

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
//    viewMap = tx.newInMemoryIdMap[ElemHandle]
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

      logA(s"pattern processPrepare($spanP, $timeRef, $initial); time = $time")

      implicit private[this] val itx: I1#Tx = iSys(tx)

      private[this] val st: St = streamRef.get(tx.peer) match {
        case Some(_st) if !initial && time == spanP.start =>
//          println("(reuse)")
          _st
        case _ =>
//          println("(new expansion)")
          val _ctx: Ctx = patterns.lucre.Context.dual[S](patObj)
          patContext.update(_ctx)(tx.peer)
          val _st: St = _ctx.expandDual(patObj.value) // g.expand[I1]
          streamRef.swap(Some(_st))(tx.peer).foreach(_.dispose())
          time = 0L
          _st
      }

      implicit private[this] val ctx: Ctx = patContext.get(tx.peer)

      private[this] var patSpan: Span = _
      private[this] var patModel: Model = _ // AuralObj[S] = _

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
              if (delta >= 0.0) {
                val sustain     = Event.sustain(evt)
                val deltaFrames = (TimeRef.SampleRate * delta).toLong
                val durFrames   = math.max(32L, (TimeRef.SampleRate * sustain).toLong)    // XXX TODO hard-coded min
                patSpan         = Span(time, time + durFrames)
                time += deltaFrames
                streamPos.set(time)(tx.peer)

                evt.get(Event.keyPlay) match {
                  case Some(key: String) =>
                    patObj.attr.get(key) match {
                      case Some(playObj) =>
                        val hit = if (initial) patSpan.overlaps(spanP) else patSpan.start >= spanP.start
                        if (hit) {
//                          patModel = AuralObj(playObj)
                          patModel = (evt, playObj)
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
          if (!ctx.hasNext(st)) streamPos.set(Long.MinValue)(tx.peer)
          if (countLoop >= 100) Console.err.println("Pattern does not advance (100 elements counted)")
        }
      }

      def next(): (Target, SpanLike, Model) = {
        if (!_hasNext) Iterator.empty.next()
        val res = ((), patSpan, patModel)
        countLoop = 0
        advance()
        res
      }

      // constructor
      {
        advance()
//        streamPos.set(spanP.stop)(tx.peer)
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
//    val foo = offset / TimeRef.SampleRate
//    math.max(0L, offset + 1)
    val p = streamPos.get(tx.peer)
//    val bar = p / TimeRef.SampleRate
    if (p > offset) p else Long.MaxValue
  }

  @inline
  private[this] def intersect(offset: Long)(implicit tx: S#Tx): Iterator[Leaf] =
    BiGroupImpl.intersectTime(tree)(offset)(iSys(tx))

  protected def processPlay(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    val toStart = intersect(timeRef.offset)
    playViews(toStart, timeRef, target)
  }

  // XXX TODO -- DRY with AuralTimelineBase
  protected def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    //    val (toStartI, toStopI) = eventsAt(timeRef.offset)

    val itx       = iSys(tx)
    val stopShape = LongRectangle(BiGroup.MinCoordinate, timeRef.offset, BiGroup.MaxSide, 1)
    val toStop    = tree.rangeQuery(stopShape )(itx)

    // this is a pretty tricky decision...
    // do we first free the stopped views and then launch the started ones?
    // or vice versa?
    //
    // we stick now to stop-then-start because it seems advantageous
    // for aural-attr-target as we don't build up unnecessary temporary
    // attr-set/attr-map synths. however, I'm not sure this doesn't
    // cause a problem where the stop action schedules on immediate
    // bundle and the start action requires a sync'ed bundle? or is
    // this currently prevented automatically? we might have to
    // reverse this decision.

    // N.B.: as crucial is to understand that the iterators from `rangeQuery` may become
    // invalid if modifying the tree while iterating. Thus, we first create only the
    // `toStop` iterator, and if i not empty, force it to a stable collection. Only after
    // stopping the views, we create the `toStart` iterator which might otherwise have
    // become invalid as well. (Mellite bug #71).

    //        playViews(toStart, tr, play.target)
    //        stopAndDisposeViews(toStop)

    if (toStop.hasNext) {
      // N.B. `toList` to avoid iterator invalidation
      toStop.toList.foreach { case (span, views) =>
        views.foreach { view => // case (idH, view) =>
          stopView(ElemHandle(/* idH, */ span, view))
        }
      }
    }

    val startShape  = LongRectangle(timeRef.offset, BiGroup.MinCoordinate, 1, BiGroup.MaxSide)
    val toStart     = tree.rangeQuery(startShape)(itx)

    playViews(toStart, timeRef, play.target)
  }

  protected def elemFromHandle(h: ElemHandle): Elem = h.view

  /** Should create a new view for the given object
    * and return a handle to it. As a side effect should
    * also memorize the view in a view-tree, if such structure is maintained,
    * for later retrieval in `viewEventAfter`
    */
  protected def mkView(vid: Unit, span: SpanLike, m: Model)(implicit tx: S#Tx): ElemHandle = {
    val (evt, playObj) = m
    val childView = playObj match {
      case p: Proc[S] => new AuralProcEvtImpl[S](evt).init(p)
      case _          => AuralObj(playObj)
    }
    val h         = ElemHandle[S, Elem](span, childView)
    logA(s"pattern - mkView: $span, $childView")
//    viewMap.put(tid, h)
    tree.transformAt(spanToPoint(span)) { opt =>
      // import expr.IdentifierSerializer
      val tup       = childView // (idH, childView)
      val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
      Some(newViews)
    } (iSys(tx))
    h
  }

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: S#Tx): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.span.compareStart(currentOffset) > 0 && h.span.compareStart(oldTarget) === 0
    }

  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: Target)
                        (implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    logA(s"pattern - playView: $view - $timeRef (${hashCode().toHexString})")
    view.run(timeRef, target)
    playingRef.add(h)
//    viewPlaying(h)
  }

  private def playViews(it: Iterator[Leaf], timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit =
    if (it.hasNext) it.foreach { case (span, views) =>
      val tr = timeRef.child(span)
      views.foreach { elem => // case (idH, elem) =>
        playView(ElemHandle(/* idH, */ span, elem), tr, target)
      }
    }

//  /** A notification method that may be used to `fire` an event
//    * such as `AuralObj.Timeline.ViewAdded`.
//    */
//  protected def viewPlaying(h: ElemHandle)(implicit tx: S#Tx): Unit = ()
//
//  /** A notification method that may be used to `fire` an event
//    * such as `AuralObj.Timeline.ViewRemoved`.
//    */
//  protected def viewStopped(h: ElemHandle)(implicit tx: S#Tx): Unit = ()

  protected def stopView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    logA(s"pattern - stopView: $view (${hashCode().toHexString})")
    view.stop()
    view.dispose()
    playingRef.remove(h)
    removeView(h)
  }

  protected def stopViews()(implicit tx: S#Tx): Unit = {
    // Regarding https://git.iem.at/sciss/SoundProcesses/issues/63
    // - processPrepare may be called more than once
    // - it calls `stopViews`
    // - but previously prepared views are not in playingRef
    // - they reside still in the tree
    // - therefore, instead of clearing playingRef, clear the entire tree

//    playingRef.foreach { h =>
//      stopView(h)
//    }
    val itx = iSys(tx)
    tree.iterator(itx).foreach { case (_, vec) =>
      vec.foreach { view =>
        view.stop()
        view.dispose()
      }
    }
    playingRef.clear()
    tree      .clear()(itx)
    disposeStream()
  }

  private def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    import h._
//    logA(s"pattern - removeView - $span - $view")

    // note: this doesn't have to check for `IPreparing`, as it is called only
    // via `eventReached`, thus during playing. correct?

    // preparingViews.remove(view).foreach(_.dispose())
    tree.transformAt(spanToPoint(span)) { opt =>
      opt.flatMap { case (span1, views) =>
        val i = views.indexOf(view) // indexWhere(_._2 == view)
        val views1 = if (i >= 0) {
          views.patch(i, Nil, 1)
        } else {
          Console.err.println(s"Warning: pattern - removeView - view for $objH not in tree")
          views
        }
        if (views1.isEmpty) None else Some(span1 -> views1)
      }
    } (iSys(tx))

//    viewMap.remove(idH())
  }
}
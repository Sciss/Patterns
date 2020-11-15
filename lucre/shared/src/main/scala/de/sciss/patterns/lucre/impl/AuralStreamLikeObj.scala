/*
 *  AuralPatternObj.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.{LongPoint2D, LongPoint2DLike, LongRectangle, LongSquare}
import de.sciss.lucre.impl.{BiGroupImpl, DummyTFormat}
import de.sciss.lucre.synth.Txn
import de.sciss.lucre.{BiGroup, Obj, Source, Txn => LTxn}
import de.sciss.patterns.Event
import de.sciss.serial.TFormat
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.UGenSource.Vec
import de.sciss.proc.impl.AuralScheduledBase
import de.sciss.proc.impl.AuralTimelineBase.spanToPoint
import de.sciss.proc.{AuralContext, AuralObj, Runner, TimeRef}
import de.sciss.proc.SoundProcesses.{logAural => logA}

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.concurrent.stm.{Ref, TSet}

object AuralStreamLikeObj {
  private type Leaf[T <: Txn[T]] = (SpanLike, Vec[AuralObj[T]])

  def mkTree[T <: Txn[T], I <: LTxn[I]]()(implicit tx: T, iSys: T => I):
      SkipOctree[I, LongPoint2DLike, LongSquare, (SpanLike, Vec[AuralObj[T]])] = {

    implicit val itx: I = iSys(tx)
    implicit val pointView: (Leaf[T], I) => LongPoint2D = (l, _) => spanToPoint(l._1)
    implicit val dummyKeyFmt: TFormat[I, Leaf[T]] = DummyTFormat.apply[I, Leaf[T]]
    import de.sciss.lucre.geom.LongSpace.TwoDim
    val tree = SkipOctree.empty[I, LongPoint2DLike, LongSquare, Leaf[T]](BiGroup.MaxSquare)
    tree
  }

  final case class ElemHandle[T <: Txn[T], Elem](span: SpanLike, view: Elem)
}
abstract class AuralStreamLikeObj[T <: Txn[T], I1 <: LTxn[I1], R <: Obj[T]](objH: Source[T, R],
    tree: SkipOctree[I1, LongPoint2DLike, LongSquare, (SpanLike, Vec[AuralObj[T]])])
   (implicit protected val context: AuralContext[T], iSys: T => I1)
  extends AuralScheduledBase[T, Unit, AuralObj[T]]
    with AuralObj[T] {
  attr =>

//  implicit private[this] val iSys: T => I1#Tx = system.inMemoryTx
//
//  private[this] val tree: SkipOctree[I1, LongSpace.TwoDim, (SpanLike, Vec[AuralObj[T]])] = {
//    implicit val itx: I1#Tx = iSys(tx0)
//    implicit val pointView: (Leaf/*[T]*/, I1#Tx) => LongPoint2D = (l, _) => spanToPoint(l._1)
//    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, Leaf/*[T]*/] = DummySerializerFactory[I1].dummySerializer
//    SkipOctree.empty[I1, LongSpace.TwoDim, Leaf/*[T]*/](BiGroup.MaxSquare)
//  }

  import LTxn.peer

  type ElemHandle = AuralStreamLikeObj.ElemHandle[T, Elem]

  //  def tpe: Obj.Type = Stream

  type Repr = R

  final def obj(implicit tx: T): Repr = objH()

  type ViewId     = Unit
  type Elem       = AuralObj[T]
  type Target     = Unit

  type Model      = (Event, Obj[T])

  private[this] val playingRef = TSet.empty[ElemHandle]

//  private[this] var patObserver: Disposable[T] = _

  protected type St

  private[this] val streamRef   = Ref(Option.empty[St])
  private[this] val streamPos   = Ref(0L)

  //  private type Leaf = (SpanLike, Vec[(Source[T, Ident[T]], Elem)])
  private type Leaf = (SpanLike, Vec[Elem])

  @inline
  private def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)

//  private def setPattern(g: Pat[_])(implicit tx: T): Unit = {
//    implicit val itx: I1#Tx = iSys(tx)
//    playingRef.foreach(elemRemoved(_, elemPlays = true))(tx.peer)
//    tree.clear()
//    disposeStream()
//  }

  private def disposeStream()(implicit tx: T): Unit =
    streamRef.swap(None)(tx.peer).foreach(disposeStream(_))

  protected def makeStream(r: Repr)(implicit tx: T): St

  protected def disposeStream(st: St)(implicit tx: T): Unit

  protected def streamHasNext(st: St)(implicit tx: T): Boolean

  protected def streamNext(st: St)(implicit tx: T): Any

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
  protected final def processPrepare(spanP: Span, timeRef: TimeRef, initial: Boolean)
                              (implicit tx: T): Iterator[PrepareResult] =
    new AbstractIterator[PrepareResult] {
      private[this] val patObj: Repr  = objH()
      private[this] var time  : Long  = streamPos.get(tx.peer)

      logA.debug(s"pattern processPrepare($spanP, $timeRef, $initial); time = $time")

//      implicit private[this] val itx: I1#Tx = streamSys(tx)

      private[this] val st: St = streamRef.get(tx.peer) match {
        case Some(_st) if !initial && time == spanP.start =>
          _st
        case _ =>
          val _st: St = makeStream(patObj)
          streamRef.swap(Some(_st))(tx.peer).foreach(disposeStream(_))
          time = 0L
          _st
      }

      private[this] var patSpan: Span = _
      private[this] var patModel: Model = _ // AuralObj[T] = _

      private[this] var _hasNext  = true
      private[this] var countLoop = 0     // cheesy way to avoid infinite loops

      def hasNext: Boolean = _hasNext

      @tailrec
      private def advance(): Unit = {
        countLoop += 1
        if (countLoop < 100 && time < spanP.stop && streamHasNext(st)) {
          streamNext(st) match {
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
          if (!streamHasNext(st)) streamPos.set(Long.MinValue)(tx.peer)
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
  protected final def viewEventAfter(offset: Long)(implicit tx: T): Long =
    BiGroupImpl.eventAfter(tree)(offset)(iSys(tx)).getOrElse(Long.MaxValue)

  /** Report the next interesting frame greater than the given frame for which
    * `gridReached` (internal) and `processPrepare` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected final def modelEventAfter(offset: Long)(implicit tx: T): Long = {
    //    val foo = offset / TimeRef.SampleRate
    //    math.max(0L, offset + 1)
    val p = streamPos.get(tx.peer)
    //    val bar = p / TimeRef.SampleRate
    if (p > offset) p else Long.MaxValue
  }

  @inline
  private[this] def intersect(offset: Long)(implicit tx: T): Iterator[Leaf] =
    BiGroupImpl.intersectTime(tree)(offset)(iSys(tx))

  protected final def processPlay(timeRef: TimeRef, target: Target)(implicit tx: T): Unit = {
    val toStart = intersect(timeRef.offset)
    playViews(toStart, timeRef, target)
  }

  // XXX TODO -- DRY with AuralTimelineBase
  protected final def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: T): Unit = {
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
          stopView(AuralStreamLikeObj.ElemHandle(/* idH, */ span, view))
        }
      }
    }

    val startShape  = LongRectangle(timeRef.offset, BiGroup.MinCoordinate, 1, BiGroup.MaxSide)
    val toStart     = tree.rangeQuery(startShape)(itx)

    playViews(toStart, timeRef, play.target)
  }

  protected final def elemFromHandle(h: ElemHandle): Elem = h.view

  /** Should create a new view for the given object
    * and return a handle to it. As a side effect should
    * also memorize the view in a view-tree, if such structure is maintained,
    * for later retrieval in `viewEventAfter`
    */
  protected final def mkView(vid: Unit, span: SpanLike, m: Model)(implicit tx: T): ElemHandle = {
    val (evt, playObj) = m
    val attr: Runner.Attr[T] = new EventAsRunnerMap(evt)
    val childView = AuralObj(playObj, attr = attr)
    val h         = AuralStreamLikeObj.ElemHandle[T, Elem](span, childView)
    logA.debug(s"pattern - mkView: $span, $childView")
    //    viewMap.put(tid, h)
    tree.transformAt(spanToPoint(span)) { opt =>
      // import expr.IdentifierSerializer
      val tup       = childView // (idH, childView)
      val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
      Some(newViews)
    } (iSys(tx))
    h
  }

  protected final def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: T): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.span.compareStart(currentOffset) > 0 && h.span.compareStart(oldTarget) === 0
    }

  protected final def playView(h: ElemHandle, timeRef: TimeRef.Option, target: Target)
                        (implicit tx: T): Unit = {
    val view = elemFromHandle(h)
    logA.debug(s"pattern - playView: $view - $timeRef (${hashCode().toHexString})")
    view.run(timeRef, target)
    playingRef.add(h)
    //    viewPlaying(h)
  }

  private final def playViews(it: Iterator[Leaf], timeRef: TimeRef, target: Target)(implicit tx: T): Unit =
    if (it.hasNext) it.foreach { case (span, views) =>
      val tr = timeRef.child(span)
      views.foreach { elem => // case (idH, elem) =>
        playView(AuralStreamLikeObj.ElemHandle(/* idH, */ span, elem), tr, target)
      }
    }

  //  /** A notification method that may be used to `fire` an event
  //    * such as `AuralObj.Timeline.ViewAdded`.
  //    */
  //  protected def viewPlaying(h: ElemHandle)(implicit tx: T): Unit = ()
  //
  //  /** A notification method that may be used to `fire` an event
  //    * such as `AuralObj.Timeline.ViewRemoved`.
  //    */
  //  protected def viewStopped(h: ElemHandle)(implicit tx: T): Unit = ()

  protected final def stopView(h: ElemHandle)(implicit tx: T): Unit = {
    val view = elemFromHandle(h)
    logA.debug(s"pattern - stopView: $view (${hashCode().toHexString})")
    view.stop()
    view.dispose()
    playingRef.remove(h)
    removeView(h)
  }

  protected final def stopViews()(implicit tx: T): Unit = {
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

  private final def removeView(h: ElemHandle)(implicit tx: T): Unit = {
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
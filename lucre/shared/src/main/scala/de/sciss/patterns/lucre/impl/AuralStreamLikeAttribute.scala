/*
 *  AuralPatternAttribute.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.impl.{DummyTFormat, ObservableImpl}
import de.sciss.lucre.synth.Txn
import de.sciss.lucre.{Obj, Source, Txn => LTxn}
import de.sciss.patterns.Event
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute.ViewImpl
import de.sciss.serial.TFormat
import de.sciss.span.{Span, SpanLike}
import de.sciss.proc.Runner.{State, Stopped}
import de.sciss.proc.impl.AuralScheduledBase
import de.sciss.proc.{AuralAttribute, AuralContext, ObjViewBase, Runner, TimeRef}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object AuralStreamLikeAttribute {
  def mkTree[T <: Txn[T], I <: LTxn[I]]()(implicit tx: T, iSys: T => I): SkipList.Map[I, Long, View[T]] = {
    implicit val itx: I = iSys(tx)
    implicit val dummyKeyFmt: TFormat[I, View[T]] = DummyTFormat.apply[I, View[T]]

    val tree = SkipList.Map.empty[I, Long, View[T]]
    tree
  }

  trait View[T <: Txn[T]] extends ObjViewBase[T, AuralAttribute.Target[T]] {
    def span  : Span
    def value : AuralAttribute.Scalar
    def start : Long = span.start
    def stop  : Long = span.stop
  }

  def getScalarValue(evt: Event, key: String): Option[AuralAttribute.Scalar] = evt.map.get(key).flatMap { v =>
    (getSingleFloat(v), v) match {
      case (Some(f), _) => Some(AuralAttribute.ScalarValue(f))
      case (None, xs: Seq[_]) =>
        val vecOpt = xs.foldLeft(Option(Vector.empty[Float])) {
          case (Some(ys), w) => getSingleFloat(w).map(ys :+ _)
          case _ => None
        }
        vecOpt.map(AuralAttribute.ScalarVector)

      case _ => None
    }
  }

  private def getSingleFloat(in: Any): Option[Float] = in match {
    case i: Int     => Some(i.toFloat)
    case f: Float   => Some(f)
    case d: Double  => Some(d.toFloat)
    case b: Boolean => Some(if (b) 1f else 0f)
    case _          => None
  }

  private final class ViewImpl[T <: Txn[T], R <: Obj[T]](aa: AuralAttribute[T] { type Repr = R },
                                                         val value: AuralAttribute.Scalar, val span: Span, val tpe: Obj.Type)
    extends View[T] with ObservableImpl[T, Runner.State] {

    private[this] final val stateRef = Ref[State](Stopped)

    type Repr = R

    def obj(implicit tx: T): Repr = aa.obj

    override def toString = s"AuralStreamLikeAttribute.View($value, $span)"

    def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit =
      state = Runner.Prepared

    def run(timeRef: TimeRef.Option, target: AuralAttribute.Target[T])(implicit tx: T): Unit = {
      target.put(aa, value)
      state = Runner.Running
    }

    def stop()(implicit tx: T): Unit =
      state = Runner.Stopped

    def dispose()(implicit tx: T): Unit = ()

    def state(implicit tx: T): State = stateRef()

    protected def state_=(value: State)(implicit tx: T): Unit = {
      val prev = stateRef.swap(value)
      if (value != prev) fire(value)
    }
  }
}
abstract class AuralStreamLikeAttribute[T <: Txn[T], I1 <: LTxn[I1], R <: Obj[T]](val key: String,
                                                                  objH: Source[T, R],
                                                                  observer: AuralAttribute.Observer[T],
                                                                  tree: SkipList.Map[I1, Long, AuralStreamLikeAttribute.View[T]])
                                                                 (implicit protected val context: AuralContext[T],
                                                                  protected val iSys: T => I1)
  extends AuralScheduledBase[T, AuralAttribute.Target[T], AuralStreamLikeAttribute.View[T]]
    with AuralAttribute[T] {
  attr =>

  import LTxn.peer

  type Repr = R

  final def obj(implicit tx: T): Repr = objH()

  type ViewId     = Unit
  type Elem       = AuralStreamLikeAttribute.View[T]
  type ElemHandle = Elem // AuralGraphemeBase.ElemHandle[T, Elem]
  type Target     = AuralAttribute.Target[T]
  type Model      = Elem // AuralAttribute.Scalar

  private[this] val prefChansNumRef = Ref(-2)   // -2 = cache invalid. across contents of `prefChansElemRef`
  private[this] val playingRef      = Ref(Option.empty[ElemHandle])
  private[this] val isEmptyRef      = Ref(false)

  protected type St

  private[this] val streamRef = Ref.make[St]()

  private def nextElemFromStream(time: Long)(implicit tx: T): Option[Elem] = {
    implicit val itx: I1 = iSys(tx)
    val stream = streamRef()(tx.peer)
    if (stream == null) return None

    @tailrec
    def loop(count: Int): Option[Elem] = if (count == 10 || !streamHasNext(stream)) {
      // println(if (count == 10) "-> count = 10" else "-> !stream.hasNext")
      None
    } else {
      streamNext(stream) match {
        case evt: Event =>
          AuralStreamLikeAttribute.getScalarValue(evt, Event.keyValue) match {
            case Some(v) =>
              val delta = Event.delta(evt)
              if (delta <= 0.0) {
                // println(s"-> Event.delta($evt) = 0.0")
                loop(count + 1)
              } else {
                val numFrames = (TimeRef.SampleRate * delta).toLong
                val span      = Span(time, time + numFrames)
                val view      = new ViewImpl[T, Repr](attr, v, span, tpe)
                tree.put(time, view)
                // println(s"-> $view")
                Some(view)
              }

            case None =>
              // println("-> no value")
              None
          }

        case _ =>
          // println("-> not an event pattern")
          None
      }
    }
    loop(0)
  }

  protected def makeStream(r: Repr)(implicit tx: T): St

  protected def disposeStream(st: St)(implicit tx: T): Unit

  protected def streamHasNext(st: St)(implicit tx: T): Boolean

  protected def streamNext(st: St)(implicit tx: T): Any

  protected final def setRepr(r: Repr)(implicit tx: T): Boolean = {
    // println("setGraph")
    implicit val itx: I1 = iSys(tx)
    val _pr = playingRef()(tx.peer)
    _pr.foreach(elemRemoved(_, elemPlays = true))
    //    viewTree.iterator(iSys(tx)).toList.foreach { case (_, entry) =>
    //      val elemPlays = _pr.contains(entry)
    //      elemRemoved(entry, elemPlays = elemPlays)
    //    }
    //
    //    assert(viewTree.isEmpty(iSys(tx)))
    tree.clear()

    //    import context.universe.cursor
//    implicit val _ctx: Ctx = patterns.lucre.Context.dual[T](objH()) // (system, system, itx) // InMemory()
//    patContext.update(_ctx)(tx.peer)
    val stream: St = makeStream(r) // _ctx.expandDual(g) // g.expand[I1]
    streamRef.update(stream)(tx.peer)

    val headElem  = nextElemFromStream(0L)
    val isEmpty   = headElem.isEmpty
    isEmptyRef.update(isEmpty)(tx.peer)
    val numCh     = if (isEmpty) -1 else headElem.get.value.numChannels
    val oldCh     = prefChansNumRef.swap(numCh)(tx.peer)
    if (oldCh != -2 && oldCh != numCh) {
      observer.attrNumChannelsChanged(this)
    }

    // println(s"---- prefChansNumRef() = $numCh; headElem = $headElem")
    // headElem.foreach(elem => elemAdded((), elem.span, elem.value))
    !isEmpty
  }

  final def preferredNumChannels(implicit tx: T): Int =
    prefChansNumRef()

//  def init(pat: Repr)(implicit tx: T): this.type = {
//    val graph0 = pat.value
//    setPattern(graph0)
//    patObserver = pat.changed.react { implicit tx => upd =>
//      setPattern(upd.now)
//    }
//    this
//  }

  /** Called during preparation of armed elements. This
    * happens either during initial `prepare` or during grid-events.
    * Given the `prepareSpan`, the sub-class should
    *
    * - find the elements using an `intersect`
    * - for each build a view and store it somewhere
    * - for each view call `prepareChild`
    * - accumulate the results of `prepareChild` into a `Map` that is returned.
    *
    * The map will become part of `IPreparing`. (NOT: The returned `Boolean` indicates
    * if elements were found (`true`) or not (`false`)).
    *
    * @param initial  if `true` this is an initial preparation which means the method
    *                 must include views that start before `prepareSpan` if their span
    *                 overlaps with `prepareSpan`. If `false` this is a follow up from
    *                 `gridReached` and the search must be restricted to views that
    *                 start no earlier than `prepareSpan`.
    */
  @tailrec
  protected final def processPrepare(spanP: Span, timeRef: TimeRef, initial: Boolean)
                                    (implicit tx: T): Iterator[PrepareResult] = {
    val start0 = math.max(0L, spanP.start)
    tree.floor(start0)(iSys(tx)) match {
      case Some((_, view0)) =>
        val it0: Iterator[PrepareResult] = new Iterator[PrepareResult] {
          private[this] var _next     = view0
          private[this] var _hasNext  = true

          def hasNext: Boolean = _hasNext

          private def advance(): Unit =
            if (_next.stop >= spanP.stop) {
              _hasNext = false
            } else {
              tree.get(_next.stop)(iSys(tx)) match {
                case Some(succ) => _next = succ
                case None =>
                  nextElemFromStream(_next.stop) match {
                    case Some(succ) => _next    = succ
                    case None       => _hasNext = false
                  }
              }
            }

          def next(): (Unit, SpanLike, Elem) = {
            if (!_hasNext) Iterator.empty.next()
            val res = _next
            advance()
            ((), res.span, res)
          }
        }
        if (initial) it0.dropWhile(_._3.stop < start0) else it0.dropWhile(_._3.start < start0)

      case None =>
        if (isEmptyRef()) Iterator.empty
        else {    // we lost the cache
          if (!setRepr(obj)) Iterator.empty  // became empty
          else processPrepare(spanP, timeRef, initial = initial) // repeat
        }
    }
  }

  protected final def viewEventAfter(offset: Long)(implicit tx: T): Long =
    tree.ceil(offset + 1)(iSys(tx)).fold(Long.MaxValue)(_._1)

  /** Report the next interesting frame greater than the given frame for which
    * `gridReached` (internal) and `processPrepare` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  @tailrec
  protected final def modelEventAfter(offset: Long)(implicit tx: T): Long = {
    val offsetC = math.max(-1L, offset)
    tree.floor(offsetC + 1)(iSys(tx)) match {
      case Some((time0, pred0)) =>
        if (time0 > offset) time0   // i.e. time0 == offset + 1
        else {
          val existing = viewEventAfter(offset)
          if (existing != Long.MaxValue) existing else {
            @tailrec
            def loop(pred: Elem): Long =
              nextElemFromStream(pred.stop) match {
                case Some(succ) =>
                  if (succ.start >= offset) succ.start
                  else loop(succ)
                case None => Long.MaxValue
              }

            loop(pred0)
          }
        }
      case None =>
        if (isEmptyRef()) Long.MaxValue
        else {    // we lost the cache
//          val graph = objH().value
          // println("RESETTING GRAPH [0]")
          if (!setRepr(obj)) Long.MaxValue // became empty
          else modelEventAfter(offset)  // repeat
        }
    }
  }

  protected final def processPlay(timeRef: TimeRef, target: Target)(implicit tx: T): Unit = {
    implicit val itx: I1 = iSys(tx)
    tree.floor(timeRef.offset).foreach { case (_, entry) =>
      playEntry(entry, timeRef = timeRef, target = target)
    }
  }

  private def playEntry(entry: Elem, timeRef: TimeRef, target: Target)
                       (implicit tx: T): Unit = {
    val childTime = timeRef.child(entry.span)
    playView(entry, childTime, target)
  }

  protected final def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: T): Unit = {
    val start = timeRef.offset
    val entry = tree.get(start)(iSys(tx))
      .getOrElse(throw new IllegalStateException(s"No element at event ${timeRef.offset}"))
    playEntry(entry, timeRef = timeRef, target = play.target)
  }

  protected final def elemFromHandle(h: ElemHandle): Elem = h // .view

  protected final def mkView(vid: Unit, span: SpanLike, obj: Model)(implicit tx: T): ElemHandle = obj

  protected final def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: T): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.start > currentOffset && h.start == oldTarget
    }

  protected final def playView(h: ElemHandle, timeRef: TimeRef.Option, target: AuralAttribute.Target[T])
                        (implicit tx: T): Unit = {
    val view = elemFromHandle(h)
    // logA(s"grapheme - playView: $view - $timeRef")
    stopViews()
    view.run(timeRef, target)
    playingRef() = Some(h)
  }

  protected final def stopView(h: ElemHandle)(implicit tx: T): Unit =
    if (playingRef().contains(h))
      stopViews()

  protected final def stopViews()(implicit tx: T): Unit =
    playingRef.swap(None).foreach { h =>
      val view = elemFromHandle(h)
      // logA(s"aural - stopView: $view")
      view.stop()
      view.dispose()
      removeView(h)
    }

  private final def removeView(h: ElemHandle)(implicit tx: T): Unit = {
    implicit val itx: I1 = iSys(tx)
    val start = h.start
    tree.remove(start)
    // println(s"removeView($h)")
  }
}
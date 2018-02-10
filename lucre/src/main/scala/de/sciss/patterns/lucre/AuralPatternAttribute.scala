/*
 *  AuralPatternAttribute.scala
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

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.patterns
import de.sciss.patterns.lucre.AuralPatternAttribute.ViewImpl
import de.sciss.patterns.{Event, Graph}
import de.sciss.serial.Serializer
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer}
import de.sciss.synth.proc.AuralView.{State, Stopped}
import de.sciss.synth.proc.impl.{AuralScheduledBase, DummySerializerFactory}
import de.sciss.synth.proc.{AuralAttribute, AuralContext, AuralView, AuralViewBase, TimeRef}

import scala.annotation.tailrec
import scala.concurrent.stm.{InTxn, Ref}

/*
  XXX TODO: some DRY with AuralGraphemeBase

 */
object AuralPatternAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Pattern[S]

  def typeID: Int = Pattern.typeID

  private[this] lazy val _init: Unit = AuralAttribute.addFactory(this)

  def init(): Unit = _init

  def apply[S <: Sys[S]](key: String, pat: Pattern[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](key, pat, observer, system)
    res.init(pat)
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](key: String, value: Pattern[S],
                                                      observer: Observer[S], system: S { type I = I1 })
                                                     (implicit tx: S#Tx, context: AuralContext[S]): AuralPatternAttribute[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx _
    implicit val itx: I1#Tx = iSys(tx)
    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, AuralPatternAttribute.View[S]] =
      DummySerializerFactory[I1].dummySerializer

    val tree = SkipList.Map.empty[I1, Long, AuralPatternAttribute.View[S]]
//    implicit val patCtx: patterns.Context[InTxn] = patterns.lucre.Context.InMemory()

    new AuralPatternAttribute[S, I1](key, tx.newHandle(value), observer, tree /* , viewMap */)
  }

  trait View[S <: Sys[S]] extends AuralViewBase[S, AuralAttribute.Target[S]] {
    def span  : Span
    def value : AuralAttribute.Scalar
    def start : Long = span.start
    def stop  : Long = span.stop
  }

  private final class ViewImpl[S <: Sys[S]](pat: AuralAttribute[S], val value: AuralAttribute.Scalar, val span: Span)
    extends View[S] with ObservableImpl[S, AuralView.State] {

    private[this] final val stateRef = Ref[State](Stopped)

    override def toString = s"AuralPatternAttribute.View($value, $span)"

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit =
      state = AuralView.Prepared

    def play(timeRef: TimeRef.Option, target: AuralAttribute.Target[S])(implicit tx: S#Tx): Unit = {
      target.put(pat, value)
      state = AuralView.Playing
    }

    def stop()(implicit tx: S#Tx): Unit =
      state = AuralView.Stopped

    def dispose()(implicit tx: S#Tx): Unit = ()

    def state(implicit tx: S#Tx): State = stateRef()

    protected def state_=(value: State)(implicit tx: S#Tx): Unit = {
      val prev = stateRef.swap(value)
      if (value != prev) fire(value)
    }
  }
}
final class AuralPatternAttribute[S <: Sys[S], I <: stm.Sys[I]](val key: String,
                                                                val obj: stm.Source[S#Tx, Pattern[S]],
                                                                observer: Observer[S],
                                                                viewTree: SkipList.Map[I, Long, AuralPatternAttribute.View[S]])
                                                               (implicit protected val context: AuralContext[S],
                                                                protected val iSys: S#Tx => I#Tx)
  extends AuralScheduledBase[S, AuralAttribute.Target[S], AuralPatternAttribute.View[S]]
    with AuralAttribute[S] {
  attr =>

  import TxnLike.peer

  def typeID: Int = Pattern.typeID

  type ViewID     = Unit
  type Elem       = AuralPatternAttribute.View[S]
  type ElemHandle = Elem // AuralGraphemeBase.ElemHandle[S, Elem]
  type Target     = AuralAttribute.Target[S]
  type Model      = Elem // AuralAttribute.Scalar

  private[this] val prefChansNumRef = Ref(-2)   // -2 = cache invalid. across contents of `prefChansElemRef`
  private[this] val playingRef      = Ref(Option.empty[ElemHandle])
  private[this] val isEmptyRef      = Ref(false)

  private[this] var patObserver: Disposable[S#Tx] = _

  private[this] val patContext      = Ref.make[patterns.lucre.Context.InMemory]

  private def getSingleFloat(in: Any): Option[Float] = in match {
    case i: Int     => Some(i.toFloat)
    case f: Float   => Some(f)
    case d: Double  => Some(d.toFloat)
    case b: Boolean => Some(if (b) 1f else 0f)
    case _          => None
  }

  private def getValue(evt: Event): Option[AuralAttribute.Scalar] = evt.map.get(Event.keyValue).flatMap { v =>
    (getSingleFloat(v), v) match {
      case (Some(f), _) => Some(AuralAttribute.ScalarValue(f))
      case (None, xs: Seq[_]) =>
        val vecOpt = (Option(Vector.empty[Float]) /: xs) {
          case (Some(ys), w) => getSingleFloat(w).map(ys :+ _)
          case _ => None
        }
        vecOpt.map(AuralAttribute.ScalarVector)

      case _ => None
    }
  }

  private type St = patterns.Stream[InTxn, Any]

  private[this] val streamRef = Ref[St](patterns.Stream.empty)

  private def nextElemFromStream(time: Long)(implicit tx: S#Tx): Option[Elem] = {
    val stream = streamRef()
    // println(s"nextElemFromStream($time)")

    @tailrec
    def loop(count: Int): Option[Elem] = if (count == 10 || !stream.hasNext) {
      // println(if (count == 10) "-> count = 10" else "-> !stream.hasNext")
      None
    } else {
      stream.next() match {
        case evt: Event =>
          getValue(evt) match {
            case Some(v) =>
              val delta = Event.delta(evt)
              if (delta <= 0.0) {
                // println(s"-> Event.delta($evt) = 0.0")
                loop(count + 1)
              } else {
                val numFrames = (TimeRef.SampleRate * delta).toLong
                val span      = Span(time, time + numFrames)
                val view      = new ViewImpl(attr, v, span)
                viewTree.add(time -> view)(iSys(tx))
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

  private def setGraph(g: Graph[_])(implicit tx: S#Tx): Boolean = {
    // println("setGraph")
    val _pr = playingRef()
    _pr.foreach(elemRemoved(_, elemPlays = true))
//    viewTree.iterator(iSys(tx)).toList.foreach { case (_, entry) =>
//      val elemPlays = _pr.contains(entry)
//      elemRemoved(entry, elemPlays = elemPlays)
//    }
//
//    assert(viewTree.isEmpty(iSys(tx)))
    viewTree.clear()(iSys(tx))

    implicit val _ctx: Context.InMemory = patterns.lucre.Context.InMemory()
    patContext() = _ctx
    val stream = g.expand[InTxn]
    streamRef() = stream

    val headElem  = nextElemFromStream(0L)
    val isEmpty   = headElem.isEmpty
    isEmptyRef()  = isEmpty
    val numCh     = if (isEmpty) -1 else headElem.get.value.numChannels
    val oldCh     = prefChansNumRef.swap(numCh)
    if (oldCh != -2 && oldCh != numCh) {
      observer.attrNumChannelsChanged(this)
    }

    // println(s"---- prefChansNumRef() = $numCh; headElem = $headElem")
    // headElem.foreach(elem => elemAdded((), elem.span, elem.value))
    !isEmpty
  }

  def preferredNumChannels(implicit tx: S#Tx): Int =
    prefChansNumRef()

  def init(pat: Pattern[S])(implicit tx: S#Tx): this.type = {
    val graph0 = pat.graph.value
    setGraph(graph0)
    patObserver = pat.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Pattern.GraphChange(ch) => setGraph(ch.now)
        case _ =>
      }
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
    * The map will become part of `IPreparing`. (NOT: The returned `Boolean` indicates
    * if elements were found (`true`) or not (`false`)).
    *
    * @param initial  if `true` this is an initial preparation which means the method
    *                 must include views that start before `prepareSpan` if their span
    *                 overlaps with `prepareSpan`. If `false` this is a follow up from
    *                 `gridReached` and the search must be restricted to views that
    *                 start no earlier than `prepareSpan`.
    */
  protected def processPrepare(spanP: Span, timeRef: TimeRef, initial: Boolean)
                              (implicit tx: S#Tx): Iterator[PrepareResult] = {
    val start0 = math.max(0L, spanP.start)
    viewTree.floor(start0)(iSys(tx)) match {
      case Some((_, view0)) =>
        val it0 = new Iterator[PrepareResult] {
          private[this] var _next     = view0
          private[this] var _hasNext  = true

          def hasNext: Boolean = _hasNext

          private def advance(): Unit =
            if (_next.stop >= spanP.stop) {
              _hasNext = false
            } else {
              viewTree.get(_next.stop)(iSys(tx)) match {
                case Some(succ) => _next = succ
                case None =>
                  nextElemFromStream(_next.stop) match {
                    case Some(succ) => _next    = succ
                    case None       => _hasNext = false
                  }
              }
            }

          def next(): (Unit, SpanLike, Elem) = {
            if (!_hasNext) Iterator.empty.next
            val res = _next
            advance()
            ((), res.span, res)
          }
        }
        if (initial) it0.dropWhile(_._3.stop < start0) else it0.dropWhile(_._3.start < start0)

      case None =>
        if (isEmptyRef()) Iterator.empty
        else {    // we lost the cache
          val graph = obj().graph.value
          // println("RESETTING GRAPH [1]")
          if (!setGraph(graph)) Iterator.empty  // became empty
          else processPrepare(spanP, timeRef, initial = initial) // repeat
        }
    }
  }

  protected def viewEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    viewTree.ceil(offset + 1)(iSys(tx)).fold(Long.MaxValue)(_._1)

  /** Report the next interesting frame greater than the given frame for which
    * `gridReached` (internal) and `processPrepare` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected def modelEventAfter(offset: Long)(implicit tx: S#Tx): Long = {
    val offsetC = math.max(-1L, offset)
    viewTree.floor(offsetC + 1)(iSys(tx)) match {
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
          val graph = obj().graph.value
          // println("RESETTING GRAPH [0]")
          if (!setGraph(graph)) Long.MaxValue // became empty
          else modelEventAfter(offset)  // repeat
        }
    }
  }

  protected def processPlay(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx = iSys(tx)
    viewTree.floor(timeRef.offset).foreach { case (_, entry) =>
      playEntry(entry, timeRef = timeRef, target = target)
    }
  }

  private def playEntry(entry: Elem, timeRef: TimeRef, target: Target)
                       (implicit tx: S#Tx): Unit = {
    val childTime = timeRef.child(entry.span)
    playView(entry, childTime, target)
  }

  protected def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    val start = timeRef.offset
    val entry = viewTree.get(start)(iSys(tx))
      .getOrElse(throw new IllegalStateException(s"No element at event ${timeRef.offset}"))
    playEntry(entry, timeRef = timeRef, target = play.target)
  }

  protected def elemFromHandle(h: ElemHandle): Elem = h // .view

  protected def mkView(vid: Unit, span: SpanLike, obj: Model)(implicit tx: S#Tx): ElemHandle = obj

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: S#Tx): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.start > currentOffset && h.start == oldTarget
    }

  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: AuralAttribute.Target[S])
                        (implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    // logA(s"grapheme - playView: $view - $timeRef")
    stopViews()
    view.play(timeRef, target)
    playingRef() = Some(h)
  }

  protected def stopView(h: ElemHandle)(implicit tx: S#Tx): Unit =
    if (playingRef().contains(h))
      stopViews()

  protected def stopViews()(implicit tx: S#Tx): Unit =
    playingRef.swap(None).foreach { h =>
      val view = elemFromHandle(h)
      // logA(s"aural - stopView: $view")
      view.stop()
      view.dispose()
      removeView(h)
    }

  private def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx = iSys(tx)
    val start = h.start
    viewTree.remove(start)
    // println(s"removeView($h)")
  }
}
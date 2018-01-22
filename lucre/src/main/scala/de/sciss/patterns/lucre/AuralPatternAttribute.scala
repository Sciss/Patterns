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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.patterns
import de.sciss.patterns.Event
import de.sciss.serial.Serializer
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer}
import de.sciss.synth.proc.impl.{AuralGraphemeBase, AuralScheduledBase, DummySerializerFactory}
import de.sciss.synth.proc.{AuralAttribute, AuralContext, TimeRef}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, Ref}

object AuralPatternAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Pattern[S]

  def typeID: Int = Pattern.typeID

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
    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, Vec[AuralAttribute[S]]] =
      DummySerializerFactory[I1].dummySerializer

    val tree = SkipList.Map.empty[I1, Long, Vec[AuralAttribute[S]]]
    implicit val patCtx: patterns.Context[InTxn] = patterns.lucre.Context.InMemory()

    new AuralPatternAttribute[S, I1](key, tx.newHandle(value), observer, tree /* , viewMap */)
  }
}
final class AuralPatternAttribute[S <: Sys[S], I <: stm.Sys[I]](val key: String,
                                                                 val obj: stm.Source[S#Tx, Pattern[S]],
                                                                 observer: Observer[S],
                                                                 protected val viewTree: SkipList.Map[I, Long, Vec[AuralAttribute[S]]])
                                                                (implicit protected val context: AuralContext[S],
                                                                 patContext: patterns.Context[InTxn],
                                                                 protected val iSys: S#Tx => I#Tx)
  extends AuralScheduledBase[S, AuralAttribute.Target[S], AuralAttribute[S]]
    with AuralAttribute[S] {
  attr =>

  import TxnLike.peer

  def typeID: Int = Pattern.typeID

  type ViewID     = Unit
  type Elem       = AuralAttribute[S]
  type ElemHandle = AuralGraphemeBase.ElemHandle[S, Elem]

  private[this] val prefChansNumRef   = Ref(-2)   // -2 = cache invalid. across contents of `prefChansElemRef`

  private[this] var patObserver: Disposable[S#Tx] = _

  def preferredNumChannels(implicit tx: S#Tx): Int = {
    val cache = prefChansNumRef()
    if (cache > -2) {
      // println(s"preferredNumChannels - cached: $cache")
      return cache
    }

    val pat     = obj()
    val stream  = pat.graph.value.expand[InTxn]
    if (!stream.hasNext) return -1

    val res = stream.next() match {
      case evt: Event.Out => evt.map.get("value").fold(-1) {
        case xs: Seq[_] => xs.size
        case _ => 1
      }
      case _ => return -1
    }

    prefChansNumRef() = res
    // println(s"preferredNumChannels - ${views.size} elems yield new: $res")
    res
  }

  def init(pat: Pattern[S])(implicit tx: S#Tx): this.type = {
    patObserver = pat.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Pattern.GraphChange(ch) =>
          val oldNum = prefChansNumRef.swap(-2)
          val newNum = preferredNumChannels
          if (oldNum != newNum) {
            observer.attrNumChannelsChanged(this)
          }
      }
    }
    this
  }

  type Target = AuralAttribute.Target[S]

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
  protected def processPrepare(prepareSpan: Span, timeRef: TimeRef, initial: Boolean)
                              (implicit tx: S#Tx): Iterator[PrepareResult] = ???

  // type PrepareResult = (ViewID, SpanLike, Obj[S])

  protected def viewEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    viewTree.ceil(offset + 1)(iSys(tx)).fold(Long.MaxValue)(_._1)

  protected def modelEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    ??? // obj().eventAfter(offset).getOrElse(Long.MaxValue)

  protected def processPlay(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx = iSys(tx)
    viewTree.floor(timeRef.offset).foreach { case (start, entries) =>
      playEntry(entries, start = start, timeRef = timeRef, target = target)
    }
  }

  private def playEntry(entries: Vec[Elem], start: Long, timeRef: TimeRef, target: Target)
                       (implicit tx: S#Tx): Unit = {
    // val start     = timeRef.offset
    val toStart   = entries.head
    val stop      = viewEventAfter(start)
    val span      = if (stop == Long.MaxValue) Span.From(start) else Span(start, stop)
    val h         = ElemHandle(start, toStart)
    val childTime = timeRef.child(span)
    playView(h, childTime, target)
  }

  protected def ElemHandle(start: Long, view: Elem): ElemHandle =
    AuralGraphemeBase.ElemHandle(start, view)

  protected def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: S#Tx): Unit = ???

  protected def elemFromHandle(h: ElemHandle): AuralAttribute[S] = ???

  protected def mkView(vid: ViewID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): ElemHandle = ???

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: S#Tx): Boolean = ???

  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: AuralAttribute.Target[S])
                        (implicit tx: S#Tx): Unit = ???

  protected def stopView(h: ElemHandle)(implicit tx: S#Tx): Unit = ???

  protected def stopViews()(implicit tx: S#Tx): Unit = ???
}
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
import de.sciss.serial.Serializer
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer}
import de.sciss.synth.proc.impl.{AuralScheduledBase, DummySerializerFactory}
import de.sciss.synth.proc.{AuralAttribute, AuralContext, TimeRef}

import scala.annotation.tailrec
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
    with AuralAttribute[S]
    with Observer[S] {
  attr =>

  import TxnLike.peer

  type Elem = AuralAttribute[S]

  // we sample the first encountered objects for which temporary views
  // have to built in order to get the number-of-channels. these
  // temporary views are here and must be disposed with the parent view.
  private[this] val prefChansElemRef  = Ref[Vec[Elem]](Vector.empty)
  private[this] val prefChansNumRef   = Ref(-2)   // -2 = cache invalid. across contents of `prefChansElemRef`

  private[this] var patObserver: Disposable[S#Tx] = _

  protected def makeViewElem(start: Long, child: Obj[S])(implicit tx: S#Tx): Elem = {
    val view = AuralAttribute(key, child, attr)
//    view match {
//      case ga: GraphemeAware[S] => ga.setGrapheme(start, obj())
//      case _ =>
//    }
    view
  }

  def preferredNumChannels(implicit tx: S#Tx): Int = {
    val cache = prefChansNumRef()
    if (cache > -2) {
      // println(s"preferredNumChannels - cached: $cache")
      return cache
    }

    val pat     = obj()
    val stream  = pat.graph.value.expand[InTxn]
    if (!stream.hasNext) return -1

    val view    = makeViewElem(0L, ??? /* stream.next() */)
    val views   = Vector.empty :+ view
    prefChansElemRef.swap(views).foreach(_.dispose())

    @tailrec
    def loop(views: Vec[Elem], res: Int): Int = views match {
      case head +: tail =>
        val ch = head.preferredNumChannels
        // if there is any child with `-1` (undefined), we have to return
        if (ch == -1) ch else loop(tail, math.max(res, ch))
      case _ => res
    }

    val res = loop(views, -1)
    prefChansNumRef() = res
    // println(s"preferredNumChannels - ${views.size} elems yield new: $res")
    res
  }

  // if cache is affected, simply forward, so that cache is rebuilt.
  def attrNumChannelsChanged(attr: Elem)(implicit tx: S#Tx): Unit =
    if (prefChansElemRef().contains(attr)) {  // then invalidate, otherwise ignore (what can we do?)
      prefChansNumRef() = -2
      observer.attrNumChannelsChanged(this)
    }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    prefChansElemRef.swap(Vector.empty).foreach(_.dispose())
  }

  def init(pat: Pattern[S])(implicit tx: S#Tx): this.type = {
    patObserver = pat.changed.react { implicit tx =>upd =>
      upd.changes.foreach {
        case Pattern.GraphChange(ch) =>
          ??? // elemAdded  (upd.pin, time, entry.value)
      }
    }
    this
  }

  // XXX TODO:
  protected def processPrepare(prepareSpan: Span, timeRef: TimeRef, initial: Boolean)
                              (implicit tx: S#Tx): Iterator[PrepareResult] = ???

  type ViewID     = Unit
  type ElemHandle = Unit

  protected def processPlay(timeRef: TimeRef, target: AuralAttribute.Target[S])(implicit tx: S#Tx): Unit = ???

  protected def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: S#Tx): Unit = ???

  protected def viewEventAfter(offset: Long)(implicit tx: S#Tx): Long = ???

  protected def modelEventAfter(offset: Long)(implicit tx: S#Tx): Long = ???

  protected def elemFromHandle(h: ElemHandle): AuralAttribute[S] = ???

  protected def mkView(vid: ViewID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): ElemHandle = ???

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)(implicit tx: S#Tx): Boolean = ???

  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: AuralAttribute.Target[S])(implicit tx: S#Tx): Unit = ???

  protected def stopView(h: ElemHandle)(implicit tx: S#Tx): Unit = ???

  protected def stopViews()(implicit tx: S#Tx): Unit = ???

  def typeID: Int = Pattern.typeID
}
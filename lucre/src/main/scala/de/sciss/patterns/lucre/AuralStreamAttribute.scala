/*
 *  AuralStreamAttribute.scala
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

package de.sciss.patterns.lucre

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute.View
import de.sciss.synth.proc.{AuralAttribute, AuralContext}

/*
  XXX TODO: some DRY with AuralGraphemeBase

 */
object AuralStreamAttribute extends AuralAttribute.Factory {
  type Repr[S <: stm.Sys[S]] = Stream[S]

  def tpe: Obj.Type = Stream

  private[this] lazy val _init: Unit = AuralAttribute.addFactory(this)

  def init(): Unit = _init

  def apply[S <: Sys[S]](key: String, pat: Stream[S], observer: AuralAttribute.Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](key, pat, observer)(tx, system, context) // IntelliJ highlight bug
    res.init(pat)
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](key: String, value: Stream[S],
                                                      observer: AuralAttribute.Observer[S])
                                                     (implicit tx: S#Tx, system: S { type I = I1 },
                                                      context: AuralContext[S]): AuralStreamAttribute[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx

    val tree = AuralStreamLikeAttribute.mkTree[S, I1]()
    new AuralStreamAttribute[S, I1](key, tx.newHandle(value), observer, tree /* , viewMap */)
  }
}
final class AuralStreamAttribute[S <: Sys[S], I1 <: stm.Sys[I1]](key: String,
                                                                  objH: stm.Source[S#Tx, Stream[S]],
                                                                  observer: AuralAttribute.Observer[S],
                                                                  tree: SkipList.Map[I1, Long, View[S]])
                                                                 (implicit context: AuralContext[S],
                                                                  iSys: S#Tx => I1#Tx)
  extends AuralStreamLikeAttribute[S, I1, Stream[S]](key, objH, observer, tree)
    with AuralAttribute[S] {
  attr =>

  def tpe: Obj.Type = Stream

  private[this] var stObserver: Disposable[S#Tx] = _

  protected type St = Stream[S]

  protected def disposeStream(st: St)(implicit tx: S#Tx): Unit = ()

  protected def makeStream(stObj: Stream[S])(implicit tx: S#Tx): St = stObj

  protected def streamHasNext(st: Stream[S])(implicit tx: S#Tx): Boolean =
    st.peer().hasNext(st.context, tx)

  protected def streamNext(st: Stream[S])(implicit tx: S#Tx): Any =
    st.peer().next()(st.context, tx)

  def init(st: Stream[S])(implicit tx: S#Tx): this.type = {
    setRepr(st)
    stObserver = st.changed.react { implicit tx =>upd =>
      setRepr(upd.stream)
    }
    this
  }

  override def dispose()(implicit tx: S#Tx): Unit = {
    stObserver.dispose()
    super.dispose()
  }
}
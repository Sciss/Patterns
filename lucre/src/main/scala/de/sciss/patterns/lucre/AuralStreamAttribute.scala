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
import de.sciss.lucre.{Disposable, Obj, Source, Txn => LTxn}
import de.sciss.lucre.synth.Txn
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute
import de.sciss.patterns.lucre.impl.AuralStreamLikeAttribute.View
import de.sciss.synth.proc.{AuralAttribute, AuralContext}

/*
  XXX TODO: some DRY with AuralGraphemeBase

 */
object AuralStreamAttribute extends AuralAttribute.Factory {
  type Repr[T <: LTxn[T]] = Stream[T]

  def tpe: Obj.Type = Stream

  private[this] lazy val _init: Unit = AuralAttribute.addFactory(this)

  def init(): Unit = _init

  def apply[T <: Txn[T]](key: String, pat: Stream[T], observer: AuralAttribute.Observer[T])
                        (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] = {
    val res = prepare[T, tx.I](key, pat, observer)(tx, tx.inMemoryBridge, context) // IntelliJ highlight bug
    res.init(pat)
  }

  private def prepare[T <: Txn[T], I1 <: LTxn[I1]](key: String, value: Stream[T],
                                                      observer: AuralAttribute.Observer[T])
                                                       (implicit tx: T, iSys: T => I1,
                                                      context: AuralContext[T]): AuralStreamAttribute[T, I1] = {
    val tree = AuralStreamLikeAttribute.mkTree[T, I1]()
    new AuralStreamAttribute[T, I1](key, tx.newHandle(value), observer, tree /* , viewMap */)
  }
}
final class AuralStreamAttribute[T <: Txn[T], I1 <: LTxn[I1]](key: String,
                                                                  objH: Source[T, Stream[T]],
                                                                  observer: AuralAttribute.Observer[T],
                                                                  tree: SkipList.Map[I1, Long, View[T]])
                                                                 (implicit context: AuralContext[T],
                                                                  iSys: T => I1)
  extends AuralStreamLikeAttribute[T, I1, Stream[T]](key, objH, observer, tree)
    with AuralAttribute[T] {
  attr =>

  def tpe: Obj.Type = Stream

  private[this] var stObserver: Disposable[T] = _

  protected type St = Stream[T]

  protected def disposeStream(st: St)(implicit tx: T): Unit = ()

  protected def makeStream(stObj: Stream[T])(implicit tx: T): St = stObj

  protected def streamHasNext(st: Stream[T])(implicit tx: T): Boolean =
    st.peer().hasNext(st.context, tx)

  protected def streamNext(st: Stream[T])(implicit tx: T): Any =
    st.peer().next()(st.context, tx)

  def init(st: Stream[T])(implicit tx: T): this.type = {
    setRepr(st)
    stObserver = st.changed.react { implicit tx =>upd =>
      setRepr(upd.stream)
    }
    this
  }

  override def dispose()(implicit tx: T): Unit = {
    stObserver.dispose()
    super.dispose()
  }
}
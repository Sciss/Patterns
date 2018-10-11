/*
 *  Context.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Random, Sys, TxnRandom}
import de.sciss.patterns
import de.sciss.patterns.graph.It
import de.sciss.patterns.{ContextLike, Obj, Pat, Stream}

import scala.concurrent.stm.TxnLocal

object Context {
  def apply[S <: stm.Sys[S]](implicit system: S, tx: S#Tx): patterns.Context[S] =
    new SingleImpl[S, system.I](system, tx)

  def dual[S <: stm.Sys[S]](pat: Pattern[S])(implicit system: S, tx: S#Tx): Context[S, system.I] =
    new DualImpl[S, system.I](system, tx, tx.newHandle(pat))

  object Attribute {
    final case class Key(peer: String) extends patterns.Context.Key
    final case class Value[A](peer: Option[A]) extends patterns.Context.Value {
      override def productPrefix = "Context.Attribute.Value"
    }
  }
  /** Specifies access to a an attribute's value at build time.
    *
    * @param name   name (key) of the attribute
    */
  final case class Attribute[A](name: String)(implicit val ex: Obj.Extractor[A]) extends patterns.Context.Input {
    type Key    = Attribute.Key
    type Value  = Attribute.Value[A]

    def key = Attribute.Key(name)

    override def productPrefix = "Context.Attribute"

    def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Value = {
      val peer = ex.extract(obj)
      Attribute.Value(peer)
    }

    def none: Value = Attribute.Value(None)
  }

  private abstract class Impl[S <: stm.Sys[S], I1 <: stm.Sys[I1]](tx0: S#Tx)
    extends ContextLike[S](tx0) {

    protected def i(tx: S#Tx): I1#Tx

    private[this] val id: I1#Id = i(tx0).newId()

    private[this] val seedRnd = Random[I1](id)(i(tx0))
    private[this] val tokenId = i(tx0).newIntVar(id, 1000000000) // 0x40000000

    protected final def nextSeed()(implicit tx: S#Tx): Long = {
      implicit val itx: I1#Tx = i(tx)
      seedRnd.nextLong()
    }

    protected final def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): TxnRandom[S] =
      TxnRandom[S](seed)(tx0)

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = {
      implicit val itx: I1#Tx = i(tx)
      seedRnd.setSeed(n)
    }

    final def allocToken[A]()(implicit tx: S#Tx): It[A] = {
      implicit val itx: I1#Tx = i(tx)
      val res = tokenId()
      tokenId() = res + 1
      It(res)
    }
  }

  private final class SingleImpl[S <: stm.Sys[S], I1 <: stm.Sys[I1]](system: S { type I = I1 }, tx0: S#Tx)
    extends Impl[S, I1](tx0) {

    protected def i(tx: S#Tx): I1#Tx = system.inMemoryTx(tx)
  }

  private final class DualImpl[S <: stm.Sys[S], I1 <: stm.Sys[I1]](system: S { type I = I1 }, tx0: S#Tx,
                                                                   patH: stm.Source[S#Tx, Pattern[S]])
    extends Impl[I1, I1](system.inMemoryTx(tx0)) with Context[S, I1] {

    protected def i(tx: I1#Tx): I1#Tx = tx

    def pattern(implicit tx: S#Tx): Pattern[S] = patH()

    private[this] val outer = TxnLocal[S#Tx]()

    def expandDual[A](pat: Pat[A])(implicit tx: S#Tx): Stream[I1, A] = {
      outer.set(tx)(tx.peer)
      expand[A](pat)(system.inMemoryTx(tx))
    }

    def hasNext[A](s: Stream[I1, A])(implicit tx: S#Tx): Boolean = {
      outer.set(tx)(tx.peer)
      s.hasNext(this, system.inMemoryTx(tx))
    }

    def next[A](s: Stream[I1, A])(implicit tx: S#Tx): A = {
      outer.set(tx)(tx.peer)
      s.next()(this, system.inMemoryTx(tx))
    }

    override def requestInput[V](input: patterns.Context.Input { type Value = V })(implicit tx: I1#Tx): V =
      input match {
        case a @ Context.Attribute(name) =>
          implicit val tx1: S#Tx = outer.get(tx.peer)
          val p = pattern
          val res = p.attr.get(name) match {
            case Some(value)  => a.extract[S](value)
            case None         => a.none
          }
          res

        case _ => super.requestInput(input)
      }
  }
}
trait Context[S <: Sys[S], T <: Sys[T]] extends patterns.Context[T] {
//  implicit def cursor: stm.Cursor[S]

  def pattern(implicit tx: S#Tx): Pattern[S]

  def expandDual[A](pat: Pat[A])(implicit tx: S#Tx): Stream[T, A]

  def hasNext[A](s: Stream[T, A])(implicit tx: S#Tx): Boolean

  def next[A](s: Stream[T, A])(implicit tx: S#Tx): A
}
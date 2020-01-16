/*
 *  Context.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Random, Sys, TxnRandom}
import de.sciss.patterns
import de.sciss.patterns.graph.It
import de.sciss.patterns.{ContextLike, Obj, Pat}
import de.sciss.serial.{DataInput, DataOutput, Writable}

import scala.concurrent.stm.TxnLocal

object Context {
  def apply[S <: stm.Sys[S]](implicit system: S, tx: S#Tx): patterns.Context[S] =
    new SingleImpl[S, system.I](system, tx)

  def dual[S <: stm.Sys[S]](pat: Pattern[S])(implicit system: S, tx: S#Tx): Context[S, system.I] =
    new DualImpl[S, system.I](system, tx, tx.newHandle(pat))

  def persistent[S <: stm.Sys[S]](id: S#Id)(implicit tx: S#Tx): Persistent[S] = {
    val seedRnd = TxnRandom[S]()
    val tokenId = tx.newIntVar(id, 1000000000) // 0x40000000
    new PersistentImpl[S](id, seedRnd, tokenId, tx)
  }

  private final val COOKIE = 0x5043 // "PC"

  trait Persistent[S <: stm.Sys[S]] extends patterns.Context[S] with Writable with Disposable[S#Tx] {
    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx): Persistent[Out]
  }

  def readPersistent[S <: stm.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Persistent[S] = {
    val id      = tx.readId(in, access)
    val cookie  = in.readShort()
    require (cookie == COOKIE, s"Unexpected cookie (found ${cookie.toHexString} not ${COOKIE.toHexString})")
    val seedRnd = TxnRandom.read[S](in, access)
    val tokenId = tx.readIntVar(id, in)
    new PersistentImpl[S](id, seedRnd, tokenId, tx)
  }

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

    def key: Key = Attribute.Key(name)

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

    protected def seedRnd: Random[I1#Tx]
    protected def tokenId: I1#Var[Int]

    protected final def nextSeed()(implicit tx: S#Tx): Long = {
      implicit val itx: I1#Tx = i(tx)
      seedRnd.nextLong()
    }

    protected final def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): TxnRandom[S] =
      TxnRandom[S](seed)

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

  private abstract class NewImpl[S <: stm.Sys[S], I1 <: stm.Sys[I1]](id: I1#Id, tx0: S#Tx)
    extends Impl[S, I1](tx0) {

    protected def i(tx: S#Tx): I1#Tx

//    private[this] val id: I1#Id = i(tx0).newId()

    protected final val seedRnd = Random[I1](id)(i(tx0))
    protected final val tokenId = i(tx0).newIntVar(id, 1000000000) // 0x40000000
  }

  private final class SingleImpl[S <: stm.Sys[S], I1 <: stm.Sys[I1]](system: S { type I = I1 }, tx0: S#Tx)
    extends NewImpl[S, I1](system.inMemoryTx(tx0).newId(), tx0) {

    protected def i(tx: S#Tx): I1#Tx = system.inMemoryTx(tx)
  }

  private final class PersistentImpl[S <: stm.Sys[S]](id: S#Id,
                                                      protected val seedRnd: TxnRandom[S],
                                                      protected val tokenId: S#Var[Int],
                                                      tx0: S#Tx)
    extends Impl[S, S](tx0) with Persistent[S] {

    def copy[Out <: stm.Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx): Persistent[Out] = {
      val idOut       = txOut.newId()
      val seedRndOut  = seedRnd.copy[Out]()
      val tokenIdOut  = txOut.newIntVar(idOut, tokenId())
      new PersistentImpl[Out](idOut, seedRndOut, tokenIdOut, txOut)
    }

    protected def i(tx: S#Tx): S#Tx = tx

    def write(out: DataOutput): Unit = {
      id.write(out)
      out.writeShort(COOKIE)
      seedRnd.write(out)
      tokenId.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      seedRnd.dispose()
      tokenId.dispose()
    }
  }

  private final class DualImpl[S <: stm.Sys[S], I1 <: stm.Sys[I1]](system: S { type I = I1 }, tx0: S#Tx,
                                                                   patH: stm.Source[S#Tx, Pattern[S]])
    extends NewImpl[I1, I1](system.inMemoryTx(tx0).newId(), system.inMemoryTx(tx0)) with Context[S, I1] {

    protected def i(tx: I1#Tx): I1#Tx = tx

    def pattern(implicit tx: S#Tx): Pattern[S] = patH()

    private[this] val outer = TxnLocal[S#Tx]()

    def expandDual[A](pat: Pat[A])(implicit tx: S#Tx): patterns.Stream[I1, A] = {
      outer.set(tx)(tx.peer)
      expand[A](pat)(system.inMemoryTx(tx))
    }

    def hasNext[A](s: patterns.Stream[I1, A])(implicit tx: S#Tx): Boolean = {
      outer.set(tx)(tx.peer)
      s.hasNext(this, system.inMemoryTx(tx))
    }

    def next[A](s: patterns.Stream[I1, A])(implicit tx: S#Tx): A = {
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

//  def pattern(implicit tx: S#Tx): Pattern[S]

  def expandDual[A](pat: Pat[A])(implicit tx: S#Tx): patterns.Stream[T, A]

  def hasNext[A](s: patterns.Stream[T, A])(implicit tx: S#Tx): Boolean

  def next[A](s: patterns.Stream[T, A])(implicit tx: S#Tx): A
}
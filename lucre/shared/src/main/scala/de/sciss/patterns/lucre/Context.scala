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

import de.sciss.lucre.{Disposable, Ident, Random, RandomObj, Source, Txn, Var, Obj => LObj}
import de.sciss.patterns
import de.sciss.patterns.graph.{It, Pat}
import de.sciss.patterns.{ContextLike, Obj}
import de.sciss.serial.{DataInput, DataOutput, Writable}
import de.sciss.proc.Pattern

import scala.concurrent.stm.TxnLocal

object Context {
  def apply[T <: Txn[T]]()(implicit tx: T): patterns.Context[T] =
    new SingleImpl[T, tx.I](tx, tx.inMemoryBridge)

  def dual[T <: Txn[T], I <: Txn[I]](pat: Pattern[T])(implicit tx: T, bridge: T => I): Context[T, I] =
    new DualImpl[T, I](tx, tx.newHandle(pat), bridge)

  def persistent[T <: Txn[T]](id: Ident[T])(implicit tx: T): Persistent[T] = {
    val seedRnd = RandomObj[T]()
    val tokenId = id.newIntVar(1000000000) // 0x40000000
    new PersistentImpl[T](id, seedRnd, tokenId, tx)
  }

  private final val COOKIE = 0x5043 // "PC"

  trait Persistent[T <: Txn[T]] extends patterns.Context[T] with Writable with Disposable[T] {
    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out): Persistent[Out]
  }

  def readPersistent[T <: Txn[T]](in: DataInput)(implicit tx: T): Persistent[T] = {
    val id      = tx.readId(in)
    val cookie  = in.readShort()
    require (cookie == COOKIE, s"Unexpected cookie (found ${cookie.toHexString} not ${COOKIE.toHexString})")
    val seedRnd = RandomObj.read[T](in)
    val tokenId = id.readIntVar(in)
    new PersistentImpl[T](id, seedRnd, tokenId, tx)
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

    def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Value = {
      val peer = ex.extract(obj)
      Attribute.Value(peer)
    }

    def none: Value = Attribute.Value(None)
  }

  private abstract class Impl[T <: Txn[T], I1 <: Txn[I1]](tx0: T)
    extends ContextLike[T](tx0) {

    protected def bridge: T => I1

    protected def seedRnd: Random[I1]
    protected def tokenId: Var[I1, Int]

    protected final def nextSeed()(implicit tx: T): Long = {
      implicit val itx: I1 = bridge(tx)
      seedRnd.nextLong()
    }

    protected final def mkRandomWithSeed(seed: Long)(implicit tx: T): RandomObj[T] =
      RandomObj[T](seed)

    def setRandomSeed(n: Long)(implicit tx: T): Unit = {
      implicit val itx: I1 = bridge(tx)
      seedRnd.setSeed(n)
    }

    final def allocToken[A]()(implicit tx: T): It[A] = {
      implicit val itx: I1 = bridge(tx)
      val res = tokenId()
      tokenId() = res + 1
      It(res)
    }
  }

  private abstract class NewImpl[T <: Txn[T], I <: Txn[I]](id: Ident[I], tx0: T)
    extends Impl[T, I](tx0) {

    protected final val seedRnd = RandomObj[I]()(bridge(tx0))
    protected final val tokenId = id.newIntVar(1000000000)(bridge(tx0)) // 0x40000000
  }

  private final class SingleImpl[T <: Txn[T], I1 <: Txn[I1]](tx0: T, val bridge: T => I1)
    extends NewImpl[T, I1](bridge(tx0).newId(), tx0)

  private final class PersistentImpl[T <: Txn[T]](id: Ident[T],
                                                   protected val seedRnd: RandomObj[T],
                                                   protected val tokenId: Var[T, Int],
                                                   tx0: T)
    extends Impl[T, T](tx0) with Persistent[T] {

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out): Persistent[Out] = {
      val idOut       = txOut.newId()
      val seedRndOut  = seedRnd.copy[Out]()
      val tokenIdOut  = idOut.newIntVar(tokenId())
      new PersistentImpl[Out](idOut, seedRndOut, tokenIdOut, txOut)
    }

    protected val bridge: T => T = tx => tx

    def write(out: DataOutput): Unit = {
      id.write(out)
      out.writeShort(COOKIE)
      seedRnd.write(out)
      tokenId.write(out)
    }

    def dispose()(implicit tx: T): Unit = {
      seedRnd.dispose()
      tokenId.dispose()
    }
  }

  private final class DualImpl[T <: Txn[T], I <: Txn[I]](tx0: T, patH: Source[T, Pattern[T]], bridgeT: T => I)
    extends NewImpl[I, I](bridgeT(tx0).newId(), bridgeT(tx0)) with Context[T, I] {

    protected def bridge: I => I = tx => tx   // must be a `def` because of initialization order

    def pattern(implicit tx: T): Pattern[T] = patH()

    private[this] val outer = TxnLocal[T]()

    def expandDual[A](pat: Pat[A])(implicit tx: T): patterns.Stream[I, A] = {
      outer.set(tx)(tx.peer)
      expand[A](pat)(bridgeT(tx))
    }

    def hasNext[A](s: patterns.Stream[I, A])(implicit tx: T): Boolean = {
      outer.set(tx)(tx.peer)
      s.hasNext(this, bridgeT(tx))
    }

    def next[A](s: patterns.Stream[I, A])(implicit tx: T): A = {
      outer.set(tx)(tx.peer)
      s.next()(this, bridgeT(tx))
    }

    override def requestInput[V](input: patterns.Context.Input { type Value = V })(implicit tx: I): V =
      input match {
        case a @ Context.Attribute(name) =>
          implicit val tx1: T = outer.get(tx.peer)
          val p = pattern
          val res = p.attr.get(name) match {
            case Some(value)  => a.extract[T](value)
            case None         => a.none
          }
          res

        case _ => super.requestInput(input)
      }
  }
}
trait Context[T <: Txn[T], I <: Txn[I]] extends patterns.Context[I] {

  def expandDual[A](pat: Pat[A])(implicit tx: T): patterns.Stream[I, A]

  def hasNext[A](s: patterns.Stream[I, A])(implicit tx: T): Boolean

  def next[A](s: patterns.Stream[I, A])(implicit tx: T): A
}
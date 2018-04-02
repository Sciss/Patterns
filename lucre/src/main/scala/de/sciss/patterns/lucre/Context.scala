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

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Sink, Source, Txn}
import de.sciss.patterns
import de.sciss.patterns.Context.Var
import de.sciss.patterns.graph.It
import de.sciss.patterns.{ContextLike, Random}
import de.sciss.serial.Serializer

import scala.concurrent.stm.{InTxn, Ref, TxnExecutor}

object Context {
  def InMemory(): InMemory = TxnExecutor.defaultAtomic(new InMemoryImpl(_))

  def apply[S <: stm.Sys[S]](implicit cursor: stm.Cursor[S], tx: S#Tx): Context[S#Tx] = new SysImpl[S](tx)

  trait Sys[S <: stm.Sys[S]] extends Context[S#Tx] {
    type ID   = S#ID
    type Acc  = S#Acc

    def step[A](fun: S#Tx => A): A
  }

  trait InMemory extends Context[InTxn] {
    type Tx = InTxn
    type ID = Unit

    def step[A](fun: Tx => A): A = Txn.atomic(fun)
  }

  private final class RandomImpl(peer: TxnRandom[InTxn]) extends Random[InTxn] {
    def setSeed(n: Long)(implicit tx: InTxn): Unit = peer.setSeed(n)

    def nextDouble()(implicit tx: InTxn): Double = peer.nextDouble()
    def nextLong  ()(implicit tx: InTxn): Long   = peer.nextLong()

    def nextInt(n: Int)(implicit tx: InTxn): Int = peer.nextInt(n)
  }

  private final class VarImpl[A](init: A) extends Sink[InTxn, A] with Source[InTxn, A] {
    private[this] val peer = Ref(init)

    def update(v: A)(implicit tx: InTxn): Unit = peer() = v

    def apply()(implicit tx: InTxn): A = peer()
  }

  private final class BooleanVarImpl(init: Boolean) extends Sink[InTxn, Boolean] with Source[InTxn, Boolean] {
    private[this] val peer = Ref(init)

    def update(v: Boolean)(implicit tx: InTxn): Unit = peer() = v

    def apply()(implicit tx: InTxn): Boolean = peer()
  }

  private final class IntVarImpl(init: Int) extends Sink[InTxn, Int] with Source[InTxn, Int] {
    private[this] val peer = Ref(init)

    def update(v: Int)(implicit tx: InTxn): Unit = peer() = v

    def apply()(implicit tx: InTxn): Int = peer()
  }

  private final class InMemoryImpl(tx0: InTxn) extends ContextLike[InTxn](tx0) with InMemory {
    private[this] val seedRnd = TxnRandom.plain()
    private[this] val tokenId = newVar((), 1000000000)(tx0, null) // 0x40000000

    protected def nextSeed()(implicit tx: Tx): Long = seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: Tx): Unit = seedRnd.setSeed(n)

    protected def mkRandomWithSeed(seed: Long)(implicit tx: Tx): Random[Tx] =
      new RandomImpl(TxnRandom.plain(seed))

    def newID()(implicit tx: Tx): Unit = ()

    def newVar[A](id: Unit, init: A)(implicit tx: Tx, serializer: Serializer[Tx, Acc, A]): Var[Tx, A] =
      new VarImpl[A](init)

    def newBooleanVar (id: Unit, init: Boolean)(implicit tx: Tx): Var[Tx, Boolean]  = new BooleanVarImpl(init)
    def newIntVar     (id: Unit, init: Int    )(implicit tx: Tx): Var[Tx, Int]      = new IntVarImpl    (init)

    def allocToken[A]()(implicit tx: Tx): It[A] = {
      val res = tokenId()
      tokenId() = res + 1
      It(res)
    }
  }

  private final class SysImpl[S <: stm.Sys[S]](tx0: S#Tx)(implicit cursor: stm.Cursor[S])
    extends ContextLike[S#Tx](tx0) with Sys[S] {

    private[this] val id      = tx0.newID()
    private[this] val seedRnd = TxnRandom[S](id)(tx0)
    private[this] val tokenId = newIntVar(id, 1000000000)(tx0) // 0x40000000

    protected def nextSeed()(implicit tx: S#Tx): Long = seedRnd.nextLong()

    protected def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): Random[S#Tx] = ???

    def step[A](fun: S#Tx => A): A = cursor.step(fun)

    def newID()(implicit tx: S#Tx): S#ID = tx.newID()

    def newVar[A](id: S#ID, init: A)(implicit tx: S#Tx, serializer: Serializer[S#Tx, Acc, A]): Var[S#Tx, A] =
      tx.newVar(id, init)

    def newIntVar     (id: S#ID, init: Int    )(implicit tx: S#Tx): Var[S#Tx, Int]      = tx.newIntVar    (id, init)
    def newBooleanVar (id: S#ID, init: Boolean)(implicit tx: S#Tx): Var[S#Tx, Boolean]  = tx.newBooleanVar(id, init)

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = seedRnd.setSeed(n)

    def allocToken[A]()(implicit tx: S#Tx): It[A] = {
      val res = tokenId()
      tokenId() = res + 1
      It(res)
    }
  }
}
trait Context[Tx] extends patterns.Context[Tx] {
  def step[A](fun: Tx => A): A
}
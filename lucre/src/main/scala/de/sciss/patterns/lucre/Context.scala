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
import de.sciss.lucre.stm.{Sink, Source, Sys, Txn}
import de.sciss.patterns
import de.sciss.patterns.Context.Var
import de.sciss.patterns.graph.It
import de.sciss.patterns.{ContextLike, Random}

import scala.concurrent.stm.{InTxn, Ref, TxnExecutor}

object Context {
  def InMemory(): InMemory = TxnExecutor.defaultAtomic(new InMemoryImpl(_))

  def apply[S <: Sys[S]](implicit cursor: stm.Cursor[S], tx: S#Tx): Context[S#Tx] = new SysImpl[S](tx)

  trait Transactional[Tx] extends Context[Tx] {
    def step[A](fun: Tx => A): A
  }

  trait InMemory extends Transactional[InTxn] {
    type Tx = InTxn

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

  private final class InMemoryImpl(tx0: InTxn) extends ContextLike[InTxn](tx0) with InMemory {
    private[this] val seedRnd = TxnRandom.plain()
    private[this] val tokenId = newVar(1000000000)(tx0) // 0x40000000

    protected def nextSeed()(implicit tx: Tx): Long = seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: Tx): Unit = seedRnd.setSeed(n)

    protected def mkRandomWithSeed(seed: Long)(implicit tx: Tx): Random[Tx] =
      new RandomImpl(TxnRandom.plain(seed))

    def newVar[A](init: A)(implicit tx: Tx): Var[Tx, A] = new VarImpl[A](init)

    def allocToken[A]()(implicit tx: Tx): It[A] = {
      val res = tokenId()
      tokenId() = res + 1
      It(res)
    }
  }

  private final class SysImpl[S <: Sys[S]](tx0: S#Tx)(implicit cursor: stm.Cursor[S])
    extends ContextLike[S#Tx](tx0) with Context[S#Tx] {

    protected def nextSeed()(implicit tx: S#Tx): Long = ???

    protected def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): Random[S#Tx] = ???

    def step[A](fun: S#Tx => A): A = cursor.step(fun)

    def newVar[A](init: A)(implicit tx: S#Tx): Var[S#Tx, A] = ???

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = ???

    def allocToken[A]()(implicit tx: S#Tx): It[A] = ???
  }
}
trait Context[Tx] extends patterns.Context[Tx] {
  def step[A](fun: Tx => A): A
}
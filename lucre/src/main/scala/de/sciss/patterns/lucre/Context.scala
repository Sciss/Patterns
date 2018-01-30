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
import de.sciss.lucre.stm.{Sink, Source, Txn}
import de.sciss.patterns
import de.sciss.patterns.Context.Var
import de.sciss.patterns.{ContextLike, Random}

import scala.concurrent.stm.{InTxn, Ref}

object Context {
  def InMemory(): InMemory = new InMemoryImpl

  trait InMemory extends patterns.Context[InTxn] {
    def step[A](fun: InTxn => A): A = Txn.atomic(fun)
  }

  private final class RandomImpl(peer: TxnRandom[InTxn]) extends Random[InTxn] {
    def nextDouble()(implicit tx: InTxn): Double = peer.nextDouble()
    def nextLong  ()(implicit tx: InTxn): Long   = peer.nextLong()

    def nextInt(n: Int)(implicit tx: InTxn): Int = peer.nextInt(n)
  }

  private final class VarImpl[A](init: A) extends Sink[InTxn, A] with Source[InTxn, A] {
    private[this] val peer = Ref(init)

    def update(v: A)(implicit tx: InTxn): Unit = peer() = v

    def apply()(implicit tx: InTxn): A = peer()
  }

  private final class InMemoryImpl extends ContextLike[InTxn] with InMemory {
    private[this] val seedRnd = TxnRandom.plain()

    def mkRandom()(implicit tx: InTxn): Random[InTxn] = new RandomImpl(TxnRandom.plain(seedRnd.nextLong()))

    def newVar[A](init: A): Var[InTxn, A] = new VarImpl[A](init)
  }
}
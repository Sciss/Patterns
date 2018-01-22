package de.sciss.patterns.lucre

import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.stm.Sys
import de.sciss.patterns
import de.sciss.patterns.Context.Var
import de.sciss.patterns.{ContextLike, Random}

import scala.concurrent.stm.InTxn

object Context {
  def apply[S <: Sys[S]]: InMemory = ???

  trait InMemory extends patterns.Context[InTxn]

  private final class RandomImpl(peer: TxnRandom[InTxn]) extends Random[InTxn] {
    def nextDouble()(implicit tx: InTxn): Double = peer.nextDouble()

    def nextLong()(implicit tx: InTxn): Long = peer.nextLong()

    def nextInt(n: Int)(implicit tx: InTxn): Int = peer.nextInt(n)
  }

  private final class InMemoryImpl extends ContextLike[InTxn] with InMemory {
    private[this] val seedRnd = TxnRandom.plain()

    def getOuterStream[A](token: Int)(implicit tx: InTxn): patterns.Stream[InTxn, A] = ???

    def setOuterStream[A](token: Int, outer: patterns.Stream[InTxn, A])(implicit tx: InTxn): Unit = ???

    def mkRandom()(implicit tx: InTxn): Random[InTxn] = new RandomImpl(TxnRandom.plain(seedRnd.nextLong()))

    def newVar[A](init: A): Var[InTxn, A] = ???
  }
}

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
import de.sciss.lucre.stm.{Random, Sys}
import de.sciss.patterns
import de.sciss.patterns.ContextLike
import de.sciss.patterns.graph.It

object Context {
  // def InMemory(): InMemory = TxnExecutor.defaultAtomic(new InMemoryImpl(_))

  def apply[S <: stm.Sys[S]](implicit cursor: stm.Cursor[S], tx: S#Tx): Context[S] = new SysImpl[S](tx)

//  private final class InMemoryImpl(tx0: InTxn) extends ContextLike[InTxn](tx0) with InMemory {
//    private[this] val seedRnd = TxnRandom.plain()
//    private[this] val tokenId = newVar((), 1000000000)(tx0, null) // 0x40000000
//
//    protected def nextSeed()(implicit tx: Tx): Long = seedRnd.nextLong()
//
//    def setRandomSeed(n: Long)(implicit tx: Tx): Unit = seedRnd.setSeed(n)
//
//    protected def mkRandomWithSeed(seed: Long)(implicit tx: Tx): Random[Tx] =
//      new RandomImpl(TxnRandom.plain(seed))
//
//    def newId()(implicit tx: Tx): Unit = ()
//
//    def newVar[A](id: Unit, init: A)(implicit tx: Tx, serializer: Serializer[Tx, Acc, A]): Var[Tx, A] =
//      new VarImpl[A](init)
//
//    def newBooleanVar (id: Unit, init: Boolean)(implicit tx: Tx): Var[Tx, Boolean]  = new BooleanVarImpl(init)
//    def newIntVar     (id: Unit, init: Int    )(implicit tx: Tx): Var[Tx, Int]      = new IntVarImpl    (init)
//
//    def allocToken[A]()(implicit tx: Tx): It[A] = {
//      val res = tokenId()
//      tokenId() = res + 1
//      It(res)
//    }
//  }

  private final class SysImpl[S <: stm.Sys[S]](tx0: S#Tx)(implicit val cursor: stm.Cursor[S])
    extends ContextLike[S](tx0) with Context[S] {

    private[this] val seedRnd = Random[S](id)(tx0)
    private[this] val tokenId = tx0.newIntVar(id, 1000000000) // 0x40000000

    protected def nextSeed()(implicit tx: S#Tx): Long = seedRnd.nextLong()

    protected def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): Random[S#Tx] =
      Random[S](tx.newId(), seed)(tx0)

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = seedRnd.setSeed(n)

    def allocToken[A]()(implicit tx: S#Tx): It[A] = {
      val res = tokenId()
      tokenId() = res + 1
      It(res)
    }
  }
}
trait Context[S <: Sys[S]] extends patterns.Context[S] {
  implicit def cursor: stm.Cursor[S]

//  def step[A](fun: Tx => A): A
}
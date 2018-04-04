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

package de.sciss.patterns

import de.sciss.lucre.stm.{Base, Plain}
import de.sciss.patterns.graph.It

trait Context[S <: Base[S]] {
  def addStream[A](ref: AnyRef, stream: Stream[S, A]): Stream[S, A]

  def getStreams(ref: AnyRef): List[Stream[S, _]]

  def mkOuterStream[A](token: Int)(implicit tx: S#Tx): Stream[S, A]

  def provideOuterStream[A](token: Int, outer: S#Tx => Stream[S, A])(implicit tx: S#Tx): Unit

  /** Creates a new pseudo-random number generator. */
  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: S#Tx): Random[S#Tx]

  def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit

  def allocToken[A]()(implicit tx: S#Tx): It[A]
}

object Context {
  def apply(): Context[Plain] = new PlainImpl

//  private final class PlainRandom(seed: Long) extends Random[NoB#S#Tx] {
//    private[this] val peer = new scala.util.Random(seed)
//
//    def setSeed(n: Long)(implicit tx: NoB#S#Tx): Unit = peer.setSeed(n)
//
//    def nextDouble()(implicit tx: NoB#S#Tx): Double = peer.nextDouble()
//    def nextLong  ()(implicit tx: NoB#S#Tx): Long   = peer.nextLong  ()
//
//    def nextInt(n: Int)(implicit tx: NoB#S#Tx): Int = peer.nextInt(n)
//  }

  private final class PlainImpl extends ContextLike[Plain](Plain.instance) {
    type S = Plain

    private[this] lazy val seedRnd  = ??? // new PlainRandom(System.currentTimeMillis())
    private[this] var tokenId       = 1000000000 // 0x40000000

    protected def nextSeed()(implicit tx: S#Tx): Long = ??? // seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = ??? // seedRnd.setSeed(n)

    def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): Random[S#Tx] =
      ??? // new PlainRandom(seed)

    def allocToken[A]()(implicit tx: S#Tx): It[A] = {
      val res = tokenId
      tokenId += 1
      It(res)
    }
  }
}

private[patterns] abstract class ContextLike[S <: Base[S]](tx0: S#Tx) extends Context[S] {
  private[this] var streamMap = Map.empty[AnyRef, List[Stream[S, _]]]
  private[this] val tokenMap  = ??? : S#Var[Map[Int, S#Tx => Stream[S, _]]] // newVar(Map.empty[Int, S#Tx => Stream[S, _]])(tx0)
  private[this] val seedMap   = ??? : S#Var[(Map[AnyRef, Long])] // newVar(Map.empty[AnyRef, Long])(tx0)

  protected def nextSeed()(implicit tx: S#Tx): Long

  protected def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): Random[S#Tx]

  def addStream[A](ref: AnyRef, stream: Stream[S, A]): Stream[S, A] = {
    streamMap += ref -> (stream :: streamMap.getOrElse(ref, Nil))
    stream
  }

  def getStreams(ref: AnyRef): List[Stream[S, _]] = streamMap.getOrElse(ref, Nil)

  def provideOuterStream[A](token: Int, outer: S#Tx => Stream[S, A])(implicit tx: S#Tx): Unit = {
    logStream(s"Context.provideOuterStream($token, ...)")
    tokenMap() = tokenMap() + (token -> outer)
  }

  def mkOuterStream[A](token: Int)(implicit tx: S#Tx): Stream[S, A] = {
    val fun                 = tokenMap().apply(token)
    val res0: Stream[S, _] = fun(tx)
    val res                 = res0.asInstanceOf[Stream[S, A]]
    logStream(s"Context.mkOuterStream($token) = $res")
    res
  }

  def mkRandom(ref: AnyRef)(implicit tx: S#Tx): Random[S#Tx] = {
    val m0 = seedMap()
    val seed = m0.getOrElse(ref, {
      val res = nextSeed()
      val m1 = m0 + (ref -> res)
      seedMap() = m1
      res
    })
    mkRandomWithSeed(seed)
  }
}
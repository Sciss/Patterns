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

import de.sciss.lucre.stm.{Base, Plain, Random}
import de.sciss.patterns.graph.It

trait Context[S <: Base[S]] {
  def addStream[A](ref: AnyRef, stream: Stream[S, A])(implicit tx: S#Tx): Stream[S, A]

  def getStreams(ref: AnyRef)(implicit tx: S#Tx): List[Stream[S, _]]

  def mkOuterStream[A](token: Int)(implicit tx: S#Tx): Stream[S, A]

  def provideOuterStream[A](token: Int, outer: S#Tx => Stream[S, A])(implicit tx: S#Tx): Unit

  /** Creates a new pseudo-random number generator. */
  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: S#Tx): Random[S#Tx]

  def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit

  def allocToken[A]()(implicit tx: S#Tx): It[A]
}

object Context {
  def apply(): Context[Plain] = new PlainImpl

  private final class PlainImpl extends ContextLike[Plain](Plain.instance) {
    type S = Plain

    private[this] lazy val seedRnd  = Random[Plain](id)
    private[this] var tokenId       = 1000000000 // 0x40000000

    protected def nextSeed()(implicit tx: S#Tx): Long = seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = seedRnd.setSeed(n)

    def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): Random[S#Tx] = {
      Random[S](Plain.instance.newId(), seed)
    }

    def allocToken[A]()(implicit tx: S#Tx): It[A] = {
      val res = tokenId
      tokenId += 1
      It(res)
    }
  }
}

private[patterns] abstract class ContextLike[S <: Base[S]](tx0: S#Tx) extends Context[S] {
  protected final val id: S#Id = tx0.newId()

  private[this] val streamMap = ??? : S#Var[Map[AnyRef, List[Stream[S, _]]]] // Map.empty[AnyRef, List[Stream[S, _]]]
  private[this] val tokenMap  = ??? : S#Var[Map[Int, S#Tx => Stream[S, _]]] // newVar(Map.empty[Int, S#Tx => Stream[S, _]])(tx0)
  private[this] val seedMap   = ??? : S#Var[Map[AnyRef, Long]] // newVar(Map.empty[AnyRef, Long])(tx0)

  protected def nextSeed()(implicit tx: S#Tx): Long

  protected def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): Random[S#Tx]

  def addStream[A](ref: AnyRef, stream: Stream[S, A])(implicit tx: S#Tx): Stream[S, A] = {
    val map0 = streamMap()
    val map1 = map0 + (ref -> (stream :: map0.getOrElse(ref, Nil)))
    streamMap() = map1
    stream
  }

  def getStreams(ref: AnyRef)(implicit tx: S#Tx): List[Stream[S, _]] = streamMap().getOrElse(ref, Nil)

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
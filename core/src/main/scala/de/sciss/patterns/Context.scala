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

import de.sciss.lucre.stm.{Sink, Source}
import de.sciss.patterns.Context.Var
import de.sciss.patterns.graph.It

trait Context[Tx] {
//  def visit[U](ref: AnyRef, init: => U): U

  def addStream[A](ref: AnyRef, stream: Stream[Tx, A]): Stream[Tx, A]

  def getStreams(ref: AnyRef): List[Stream[Tx, _]]

  def mkOuterStream[A](token: Int)(implicit tx: Tx): Stream[Tx, A]

  def provideOuterStream[A](token: Int, outer: Tx => Stream[Tx, A])(implicit tx: Tx): Unit

  /** Creates a new pseudo-random number generator. */
  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: Tx): Random[Tx]

  def newVar[A](init: A)(implicit tx: Tx): Var[Tx, A]

  def newIntVar     (init: Int    )(implicit tx: Tx): Var[Tx, Int     ]
  def newBooleanVar (init: Boolean)(implicit tx: Tx): Var[Tx, Boolean ]

  def setRandomSeed(n: Long)(implicit tx: Tx): Unit

  def allocToken[A]()(implicit tx: Tx): It[A]
}

object Context {
  def apply(): Plain = new PlainImpl

  type Var[Tx, A] = Sink[Tx, A] with Source[Tx, A]

  implicit object NoTx extends NoTx
  sealed trait NoTx

  trait Plain extends Context[NoTx] {
    type Tx = NoTx
  }

  private final class PlainRandom(seed: Long) extends Random[NoTx] {
    private[this] val peer = new scala.util.Random(seed)

    def setSeed(n: Long)(implicit tx: NoTx): Unit = peer.setSeed(n)

    def nextDouble()(implicit tx: NoTx): Double = peer.nextDouble()
    def nextLong  ()(implicit tx: NoTx): Long   = peer.nextLong  ()

    def nextInt(n: Int)(implicit tx: NoTx): Int = peer.nextInt(n)
  }

  private final class PlainImpl extends ContextLike[NoTx](NoTx) with Plain {
    def newVar[A]     (init: A       )(implicit tx: Tx): Var[Tx, A]        = new PlainVar[A]     (init)
    def newIntVar     (init: Int     )(implicit tx: Tx): Var[Tx, Int]      = new PlainIntVar     (init)
    def newBooleanVar (init: Boolean )(implicit tx: Tx): Var[Tx, Boolean]  = new PlainBooleanVar (init)

    private[this] lazy val seedRnd  = new PlainRandom(System.currentTimeMillis())
    private[this] var tokenId       = 1000000000 // 0x40000000

    protected def nextSeed()(implicit tx: Tx): Long = seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: Tx): Unit = seedRnd.setSeed(n)

    def mkRandomWithSeed(seed: Long)(implicit tx: Tx): Random[Tx] =
      new PlainRandom(seed)

    def allocToken[A]()(implicit tx: Tx): It[A] = {
      val res = tokenId
      tokenId += 1
      It(res)
    }
  }

  private final class PlainVar[A](private[this] var current: A) extends Sink[NoTx, A] with Source[NoTx, A] {
    override def toString: String = s"Var($current)"

    def apply()(implicit tx: NoTx): A =
      current

    def update(v: A)(implicit tx: NoTx): Unit =
      current = v
  }


  private final class PlainBooleanVar(private[this] var current: Boolean)
    extends Sink[NoTx, Boolean] with Source[NoTx, Boolean] {

    override def toString: String = s"Var($current)"

    def apply()(implicit tx: NoTx): Boolean =
      current

    def update(v: Boolean)(implicit tx: NoTx): Unit =
      current = v
  }
  
  private final class PlainIntVar(private[this] var current: Int) 
    extends Sink[NoTx, Int] with Source[NoTx, Int] {
    
    override def toString: String = s"Var($current)"

    def apply()(implicit tx: NoTx): Int =
      current

    def update(v: Int)(implicit tx: NoTx): Unit =
      current = v
  }
}

private[patterns] abstract class ContextLike[Tx](tx0: Tx) extends Context[Tx] {
  private[this] var streamMap = Map.empty[AnyRef, List[Stream[Tx, _]]]
  private[this] val tokenMap  = ??? : Var[Tx, Map[Int, Tx => Stream[Tx, _]]] // newVar(Map.empty[Int, Tx => Stream[Tx, _]])(tx0)
  private[this] val seedMap   = ??? : Var[Tx, (Map[AnyRef, Long])] // newVar(Map.empty[AnyRef, Long])(tx0)

  protected def nextSeed()(implicit tx: Tx): Long

  protected def mkRandomWithSeed(seed: Long)(implicit tx: Tx): Random[Tx]

  def addStream[A](ref: AnyRef, stream: Stream[Tx, A]): Stream[Tx, A] = {
    streamMap += ref -> (stream :: streamMap.getOrElse(ref, Nil))
    stream
  }

  def getStreams(ref: AnyRef): List[Stream[Tx, _]] = streamMap.getOrElse(ref, Nil)

  def provideOuterStream[A](token: Int, outer: Tx => Stream[Tx, A])(implicit tx: Tx): Unit = {
    logStream(s"Context.provideOuterStream($token, ...)")
    tokenMap() = tokenMap() + (token -> outer)
  }

  def mkOuterStream[A](token: Int)(implicit tx: Tx): Stream[Tx, A] = {
    val fun                 = tokenMap().apply(token)
    val res0: Stream[Tx, _] = fun(tx)
    val res                 = res0.asInstanceOf[Stream[Tx, A]]
    logStream(s"Context.mkOuterStream($token) = $res")
    res
  }

  def mkRandom(ref: AnyRef)(implicit tx: Tx): Random[Tx] = {
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
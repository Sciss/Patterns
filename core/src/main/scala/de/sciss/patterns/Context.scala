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

import de.sciss.lucre.stm.{Base, DummySerializerFactory, Plain, Random, TxnRandom}
import de.sciss.patterns.graph.It
import de.sciss.patterns.impl.StreamSerializer
import de.sciss.serial.Serializer

trait Context[S <: Base[S]] {
  def addStream[A](ref: AnyRef, stream: Stream[S, A])(implicit tx: S#Tx): Stream[S, A]

  def getStreams(ref: AnyRef)(implicit tx: S#Tx): List[Stream[S, _]]

  def mkOuterStream[A](token: Int)(implicit tx: S#Tx): Stream[S, A]

  def provideOuterStream[A](token: Int, outer: S#Tx => Stream[S, A])(implicit tx: S#Tx): Unit

  /** Creates a new pseudo-random number generator. */
  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: S#Tx): TxnRandom[S]

  def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit

  def allocToken[A]()(implicit tx: S#Tx): It[A]

  def expand[A](pat: Pat[A])(implicit tx: S#Tx): Stream[S, A]

  implicit def streamSerializer[A]: Serializer[S#Tx, S#Acc, Stream[S, A]]
}

object Context {
  def apply(): Context[Plain] = new PlainImpl

  private final class PlainImpl extends ContextLike[Plain, Plain](Plain.instance, Plain.instance) {
    type S = Plain

    private[this] lazy val seedRnd  = Random[Plain](id)
    private[this] var tokenId       = 1000000000 // 0x40000000

    protected def nextSeed()(implicit tx: S#Tx): Long = seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = seedRnd.setSeed(n)

    def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): TxnRandom[S] = {
      TxnRandom[S](seed)
    }

    def allocToken[A]()(implicit tx: S#Tx): It[A] = {
      val res = tokenId
      tokenId += 1
      It(res)
    }
  }
}

private[patterns] abstract class ContextLike[S <: Base[S], I1 <: Base[I1]](system: S { type I = I1 }, tx0: S#Tx) extends Context[S] {
  protected final def i(tx: S#Tx): I1#Tx = system.inMemoryTx(tx)

  protected final val id: I1#Id = i(tx0).newId()

  private[this] val streamSer = new StreamSerializer[S, Any]()(this)

  private[this] val serFact = DummySerializerFactory[I1]
  import serFact.dummySerializer

  private[this] val streamMap = i(tx0).newVar[Map[AnyRef, List[Stream[S, _]]]](id, Map.empty)
  private[this] val tokenMap  = i(tx0).newVar[Map[Int, S#Tx => Stream[S, _]]] (id, Map.empty)
  private[this] val seedMap   = i(tx0).newVar[Map[AnyRef, Long]]              (id, Map.empty)

  // ---- abstract ----

  def expand[A](pat: Pat[A])(implicit tx: S#Tx): Stream[S, A] = pat.expand(this, tx)

  protected def nextSeed()(implicit tx: S#Tx): Long

  protected def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): TxnRandom[S]

  // ---- impl ----

  final def streamSerializer[A]: Serializer[S#Tx, S#Acc, Stream[S, A]] =
    streamSer.asInstanceOf[StreamSerializer[S, A]]

  def addStream[A](ref: AnyRef, stream: Stream[S, A])(implicit tx: S#Tx): Stream[S, A] = {
    implicit val itx: I1#Tx = i(tx)
    val map0 = streamMap()
    val map1 = map0 + (ref -> (stream :: map0.getOrElse(ref, Nil)))
    streamMap() = map1
    stream
  }

  def getStreams(ref: AnyRef)(implicit tx: S#Tx): List[Stream[S, _]] = {
    implicit val itx: I1#Tx = i(tx)
    streamMap().getOrElse(ref, Nil)
  }

  def provideOuterStream[A](token: Int, outer: S#Tx => Stream[S, A])(implicit tx: S#Tx): Unit = {
    logStream(s"Context.provideOuterStream($token, ...)")
    implicit val itx: I1#Tx = i(tx)
    tokenMap() = tokenMap() + (token -> outer)
  }

  def mkOuterStream[A](token: Int)(implicit tx: S#Tx): Stream[S, A] = {
    implicit val itx: I1#Tx = i(tx)
    val fun                 = tokenMap().apply(token)
    val res0: Stream[S, _] = fun(tx)
    val res                 = res0.asInstanceOf[Stream[S, A]]
    logStream(s"Context.mkOuterStream($token) = $res")
    res
  }

  def mkRandom(ref: AnyRef)(implicit tx: S#Tx): TxnRandom[S] = {
    implicit val itx: I1#Tx = i(tx)
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
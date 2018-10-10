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

import de.sciss.lucre.stm.{Base, Plain, Random, TxnRandom}
import de.sciss.patterns.graph.It
import de.sciss.patterns.impl.StreamSerializer
import de.sciss.patterns.stream.{ItStream, ItStreamSource}
import de.sciss.serial.Serializer

trait Context[S <: Base[S]] {
  def mkItStream[A](token: Int)(implicit tx: S#Tx): Stream[S, A]

  /** This method should be called by an `ItStream` after it has been
    * read in de-serialization.
    */
  def registerItStream[A](it: ItStream[S, A])(implicit tx: S#Tx): Unit

  def withItSource[A, B](source: ItStreamSource[S, A])(thunk: => B)(implicit tx: S#Tx): B

  def withItSources[B](sources: ItStreamSource[S, _]*)(thunk: => B)(implicit tx: S#Tx): B

  /** Creates a new pseudo-random number generator. */
  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: S#Tx): TxnRandom[S]

  def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit

  def allocToken[A]()(implicit tx: S#Tx): It[A]

  def expand[A](pat: Pat[A])(implicit tx: S#Tx): Stream[S, A]

  implicit def streamSerializer[A]: Serializer[S#Tx, S#Acc, Stream[S, A]]
}

object Context {
  def apply(): Context[Plain] = new PlainImpl

  private final class PlainImpl extends ContextLike[Plain](Plain.instance) {
    type S = Plain

    // A random number generator used for producing _seed_ values when creating a new RNG.
    private[this] lazy val seedRnd = {
      val id: Plain.Id = Plain.instance.newId()
      Random[Plain](id)
    }

    // Note: tokens are allocated first by the graph builder, these start at zero.
    // The context token-id allocation happens when rewriting `It` terms, as happens
    // in the expansion of `FoldLeft`. To avoid collision, we simply offset here.
    // We use a decimal readable value for easier debugging.
    private[this] var tokenId = 1000000000 // approx. 0x40000000

    protected def nextSeed()(implicit tx: S#Tx): Long = seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: S#Tx): Unit = seedRnd.setSeed(n)

    def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): TxnRandom[S] =
      TxnRandom[S](seed)

    def allocToken[A]()(implicit tx: S#Tx): It[A] = {
      // stream expansion is single threaded, so we can
      // simply mutate the counter here.
      val res = tokenId
      tokenId += 1
      It(res)
    }
  }
}

private[patterns] abstract class ContextLike[S <: Base[S]](tx0: S#Tx) extends Context[S] {
//  protected final def i(tx: S#Tx): I1#Tx = system.inMemoryTx(tx)
//
//  protected final val id: I1#Id = i(tx0).newId()

  private[this] val streamSer = new StreamSerializer[S, Any]()(this)

  private[this] val tokenMap  = tx0.newInMemoryMap[Int, List[ItStreamSource[S, _]]]
  private[this] val seedMap   = tx0.newInMemoryMap[AnyRef, Long]

  // ---- abstract ----

  def expand[A](pat: Pat[A])(implicit tx: S#Tx): Stream[S, A] = pat.expand(this, tx)

  protected def nextSeed()(implicit tx: S#Tx): Long

  protected def mkRandomWithSeed(seed: Long)(implicit tx: S#Tx): TxnRandom[S]

  // ---- impl ----

  final def streamSerializer[A]: Serializer[S#Tx, S#Acc, Stream[S, A]] =
    streamSer.asInstanceOf[StreamSerializer[S, A]]

  def withItSource[A, B](source: ItStreamSource[S, A])(thunk: => B)(implicit tx: S#Tx): B = {
    val token   = source.token
    val list0   = tokenMap.get(token).getOrElse(Nil)
    val list1   = source :: list0
    tokenMap.put(token, list1)
    try {
      thunk
    } finally {
      if (list0.isEmpty) tokenMap.remove(token) else tokenMap.put(token, list0)
    }
  }

  def withItSources[B](sources: ItStreamSource[S, _]*)(thunk: => B)(implicit tx: S#Tx): B = {
    sources.foreach { source =>
      val token = source.token
      val list0 = tokenMap.get(token).getOrElse(Nil)
      val list1 = source :: list0
      tokenMap.put(token, list1)
    }
    try {
      thunk
    } finally {
      sources.foreach { source =>
        val token = source.token
        val _ :: list0 = tokenMap.get(token).get
        if (list0.isEmpty) tokenMap.remove(token) else tokenMap.put(token, list0)
      }
    }
  }

  def mkItStream[A](token: Int)(implicit tx: S#Tx): Stream[S, A] = {
    val sources             = tokenMap.get(token).get
    val source              = sources.head
    val res0: Stream[S, _]  = source.mkItStream()(this, tx)
    val res                 = res0.asInstanceOf[Stream[S, A]]
    // $COVERAGE-OFF$
    logStream(s"Context.mkItStream($token) = $res")
    // $COVERAGE-ON$
    res
  }

  def registerItStream[A](it: ItStream[S, A])(implicit tx: S#Tx): Unit = {
    // $COVERAGE-OFF$
    logStream(s"Context.registerItStream($it)")
    // $COVERAGE-ON$
    val token               = it.token
    val sources             = tokenMap.get(token).get
    val source: ItStreamSource[S, _] = sources.head
    source.asInstanceOf[ItStreamSource[S, A]].registerItStream(it)
  }

  def mkRandom(ref: AnyRef)(implicit tx: S#Tx): TxnRandom[S] = {
    val seed = seedMap.get(ref).getOrElse {
      val res = nextSeed()
      seedMap.put(ref, res)
      res
    }
    mkRandomWithSeed(seed)
  }
}
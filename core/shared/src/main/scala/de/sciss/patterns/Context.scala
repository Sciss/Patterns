/*
 *  Context.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.lucre.{Exec, Plain, RandomObj}
import de.sciss.patterns.graph.It
import de.sciss.patterns.impl.StreamFormat
import de.sciss.patterns.stream.{ItStream, ItStreamSource}
import de.sciss.serial.TFormat

import scala.util.control.ControlThrowable

trait Context[T <: Exec[T]] {
  /** This method should be called by an `ItStream` when it is expanded (new).
    * Looks up the `ItStreamSource` for the token, and invokes its own `mkItStream`.
    */
  def mkItStream[A](token: Int)(implicit tx: T): Stream[T, A]

  /** This method should be called by an `ItStream` after it has been
    * read in de-serialization.
    * Looks up the `ItStreamSource` for `it.token`, and invokes its own `registerItStream`.
    */
  def registerItStream[A](it: ItStream[T, A])(implicit tx: T): Unit

  def withItSource[A, B](source: ItStreamSource[T, A])(thunk: => B)(implicit tx: T): B

  def withItSources[B](sources: ItStreamSource[T, _]*)(thunk: => B)(implicit tx: T): B

  /** Creates a new pseudo-random number generator. */
  def mkRandom(ref: AnyRef /* seed: Long = -1L */)(implicit tx: T): RandomObj[T]

  def setRandomSeed(n: Long)(implicit tx: T): Unit

  def allocToken[A]()(implicit tx: T): It[A]

  def expand[A](pat: Pat[A])(implicit tx: T): Stream[T, A]

  def requestInput[V](input: Context.Input { type Value = V })(implicit tx: T): V

  implicit def streamFormat[A]: TFormat[T, Stream[T, A]]
}

object Context {
  def apply(): Context[Plain] = new PlainImpl

  /** A pure marker trait to rule out some type errors. */
  trait Key
//  /** A scalar value found in the attribute map. */
//  final case class AttributeKey(name: String) extends Key
  /** A pure marker trait to rule out some type errors. */
  trait Value

  case object Unit extends Value
  type Unit = Unit.type

//  object Input {
//    object Attribute {
//      final case class Value(peer: Option[Any]) extends Context.Value {
//        override def productPrefix = "Input.Attribute.Value"
//      }
//    }
//    /** Specifies access to a an attribute's value at build time.
//      *
//      * @param name   name (key) of the attribute
//      */
//    final case class Attribute(name: String) extends Input {
//      type Key    = AttributeKey
//      type Value  = Attribute.Value
//
//      def key = AttributeKey(name)
//
//      override def productPrefix = "Input.Attribute"
//    }
//
//    object Action {
//      /** An "untyped" action reference, i.e. without system type and transactions revealed */
//      trait Value extends Context.Value {
//        def key: String
//        def execute(value: Any): scala.Unit
//      }
//    }
//    /** Specifies access to an action.
//      *
//      * @param name   name (key) of the attribute referring to an action
//      */
//    final case class Action(name: String) extends Input {
//      type Key    = AttributeKey
//      type Value  = Action.Value
//
//      def key = AttributeKey(name)
//
//      override def productPrefix = "Input.Action"
//    }
//  }
  trait Input {
    type Key   <: Context.Key
    type Value <: Context.Value

    def key: Key
  }

  final case class MissingIn(input: Input) extends ControlThrowable

  private final class PlainImpl extends ContextLike[Plain](Plain.instance) {
    type S = Plain
    type T = Plain

    // A random number generator used for producing _seed_ values when creating a new RNG.
    private[this] lazy val seedRnd = {
//      val id: Plain.Id = Plain.instance.newId()
      RandomObj[Plain]()
    }

    // Note: tokens are allocated first by the graph builder, these start at zero.
    // The context token-id allocation happens when rewriting `It` terms, as happens
    // in the expansion of `FoldLeft`. To avoid collision, we simply offset here.
    // We use a decimal readable value for easier debugging.
    private[this] var tokenId = 1000000000 // approx. 0x40000000

    protected def nextSeed()(implicit tx: T): Long = seedRnd.nextLong()

    def setRandomSeed(n: Long)(implicit tx: T): scala.Unit = seedRnd.setSeed(n)

    def mkRandomWithSeed(seed: Long)(implicit tx: T): RandomObj[T] =
      RandomObj[T](seed)

    def allocToken[A]()(implicit tx: T): It[A] = {
      // stream expansion is single threaded, so we can
      // simply mutate the counter here.
      val res = tokenId
      tokenId += 1
      It(res)
    }
  }
}

private[patterns] abstract class ContextLike[T <: Exec[T]](tx0: T) extends Context[T] {
//  protected final def i(tx: T): I1 = system.inMemoryTx(tx)
//
//  protected final val id: I1#Id = i(tx0).newId()

  private[this] val streamFmt = new StreamFormat[T, Any]()(this)

  private[this] val tokenMap  = tx0.newInMemoryMap[Int, List[ItStreamSource[T, _]]]
  private[this] val seedMap   = tx0.newInMemoryMap[AnyRef, Long]

  // ---- abstract ----

  def expand[A](pat: Pat[A])(implicit tx: T): Stream[T, A] = pat.expand(this, tx)

  protected def nextSeed()(implicit tx: T): Long

  protected def mkRandomWithSeed(seed: Long)(implicit tx: T): RandomObj[T]

  // ---- impl ----

  /** Default implementation just throws `MissingIn` */
  def requestInput[V](input: Context.Input { type Value = V })(implicit tx: T): V =
    throw Context.MissingIn(input)

  final def streamFormat[A]: TFormat[T, Stream[T, A]] =
    streamFmt.asInstanceOf[StreamFormat[T, A]]

  def withItSource[A, B](source: ItStreamSource[T, A])(thunk: => B)(implicit tx: T): B = {
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

  def withItSources[B](sources: ItStreamSource[T, _]*)(thunk: => B)(implicit tx: T): B = {
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

  def mkItStream[A](token: Int)(implicit tx: T): Stream[T, A] = {
    val sources             = tokenMap.get(token).get
    val source              = sources.head
    val res0: Stream[T, _]  = source.mkItStream()(this, tx)
    val res                 = res0.asInstanceOf[Stream[T, A]]
    logStream(s"Context.mkItStream($token) = $res")
    res
  }

  def registerItStream[A](it: ItStream[T, A])(implicit tx: T): Unit = {
    logStream(s"Context.registerItStream($it)")
    val token               = it.token
    val sources             = tokenMap.get(token).get
    val source: ItStreamSource[T, _] = sources.head
    source.asInstanceOf[ItStreamSource[T, A]].registerItStream(it)
  }

  def mkRandom(ref: AnyRef)(implicit tx: T): RandomObj[T] = {
    val seed = seedMap.get(ref).getOrElse {
      val res = nextSeed()
      seedMap.put(ref, res)
      res
    }
    mkRandomWithSeed(seed)
  }
}
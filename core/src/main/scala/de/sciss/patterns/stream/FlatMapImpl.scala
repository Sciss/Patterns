/*
 *  FlatMapImpl.scala
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
package stream

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.FlatMap
import de.sciss.serial.{DataInput, DataOutput}

object FlatMapImpl extends StreamFactory {
  final val typeId = 0x464D6170 // "FMap"

  def expand[S <: Base[S], A1, A](pat: FlatMap[A1, A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    import pat._

    ctx.allocToken()

    val id          = tx.newId()
//    val inStream    = in  .expand(ctx, tx)
//    val idxStream   = idx .expand(ctx, tx)
//    val elemStream  = elem.expand(ctx, tx)
//    val state       = tx.newVar[Stream[S, A]](id, null)
//    val valid       = tx.newBooleanVar(id, false)
//
//    new StreamImpl[S, A1, A](id = id, inStream = inStream, idxStream = idxStream, elemStream = elemStream,
//      state = state, valid = valid)
    ???
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id          = tx.readId(in, access)
    val inStream    = Stream.read[S, Any](in, access)
    val idxStream   = Stream.read[S, Int](in, access)
    val elemStream  = Stream.read[S, Any](in, access)
    val state       = tx.readVar[Stream[S, Any]](id, in)
    val valid       = tx.readBooleanVar(id, in)

//    new StreamImpl[S, Any, Any](id = id, inStream = inStream, idxStream = idxStream, elemStream = elemStream,
//      state = state, valid = valid)
    ???
  }

//  private final class ItStreamSourceImpl[S <: Base[S], A](tx0: S#Tx) extends ItStreamSource[S, Pat[A]] {
//    private[this] val set = tx0.newVar[Set[Stream[S, Pat[A]]]](???, ???)
//
//    def mkItStream()(implicit tx: S#Tx): Stream[S, Pat[A]] = ???
//
//    def pingFromIt(stream: Stream[S, Pat[A]])(implicit tx: S#Tx): Unit = ???
//  }

  private final class StreamImpl[S <: Base[S] { type I = I1 }, I1 <: Base[I1], A1, A](
    ctx0: Context[S], tx0: S#Tx, tokenId: Int,
    outer: Pat[Pat[A]]
  )
    extends Stream[S, A] with ItStreamSource[S, A] {

    @transient final private[this] lazy val ref = new AnyRef

    protected def typeId: Int = FlatMapImpl.typeId

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    private[this] val mapItStreams = ctx0.mkInMemorySet[Stream[S, A]]

    def mkItStream()(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
      val res = new MapItStream[S, A](outer, tx)
      mapItStreams.add(res)
      res
    }

    def pingFromIt(stream: Stream[S, A])(implicit tx: S#Tx): Unit = ???

//    private def mkItStream(implicit tx: S#Tx) = {
//      val res = new MapItStream(outer, tx)
//      ctx0.addStream(ref, res)
//      res
//    }

//    ctx0.provideOuterStream(it.token, mkItStream(_))(tx0)

    ???
//    private[this] val innerStream: Stream[S, A] = ctx0.withOuterStream(tokenId)(inner.expand(ctx0, tx0))

    // because `inner` is not guaranteed to depend on `It`, we must
    // pro-active create one instance of the it-stream which is used
    // as an additional constraint to determine `hasNext`!
//    private[this] val itStream      = mkItStream(tx0)

    def reset()(implicit tx: S#Tx): Unit = {
      logStream("FlatMap.iterator.reset()")
      ???
//      ctx.getStreams(ref).foreach {
//        case m: MapItStream[S, _] => m.resetOuter()
//      }
//      innerStream.reset()
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean =
      ??? // itStream.hasNext && innerStream.hasNext

    private def advance()(implicit tx: S#Tx): Unit = {
      logStream("FlatMap.iterator.advance()")
      ???
//      ctx.getStreams(ref).foreach {
//        case m: MapItStream[S, _] => m.advance()
//      }
//      innerStream.reset()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
//      val res = innerStream.next()
//      logStream(s"FlatMap.iterator.next() = $res; innerStream.hasNext = ${innerStream.hasNext}; itStream.hasNext = ${itStream.hasNext}")
//      if (!innerStream.hasNext && itStream.hasNext) advance()
//      res
      ???
    }
  }
}

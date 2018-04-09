/*
 *  Stream.scala
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

import de.sciss.lucre.stm.{Base, Disposable, Plain}
import de.sciss.serial.{DataInput, DataOutput, Serializer, Writable}

import scala.annotation.switch
import scala.collection.AbstractIterator

object Stream {
  def exhausted(): Nothing = throw new NoSuchElementException("next on empty iterator")

  implicit def serializer[S <: Base[S], A]: Serializer[S#Tx, S#Acc, Stream[S, A]] = anySer.asInstanceOf[Ser[S, A]]

  def read[S <: Base[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] =
    serializer[S, A].read(in, access)

  private val anySer = new Ser[Plain, Any]

  private final class Ser[S <: Base[S], A] extends Serializer[S#Tx, S#Acc, Stream[S, A]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Stream[S, A] = {
      val typeId = in.readInt()
      import stream._
      val f: StreamFactory = (typeId: @switch) match {
        case ApplyImpl        .typeId => ApplyImpl
        case ArithmSeqImpl    .typeId => ArithmSeqImpl
        case BinaryOpImpl     .typeId => BinaryOpImpl
        case BindImpl         .typeId => BindImpl
        case BrownImpl        .typeId => BrownImpl
        case BubbleImpl       .typeId => BubbleImpl
        case CatImpl          .typeId => CatImpl
        case ChooseImpl       .typeId => ChooseImpl
        case CombinationsImpl .typeId => CombinationsImpl
        case ConstantImpl     .typeId => ConstantImpl
        case DifferentiateImpl.typeId => DifferentiateImpl
        case DistinctImpl     .typeId => DistinctImpl
        case DropImpl         .typeId => DropImpl
        case ExpExpImpl       .typeId => ExpExpImpl
        case ExpLinImpl       .typeId => ExpLinImpl
        case FlattenImpl      .typeId => FlattenImpl
        case FormatImpl       .typeId => FormatImpl
        case GateImpl         .typeId => GateImpl
        case GeomSeqImpl      .typeId => GeomSeqImpl
        case GroupedImpl      .typeId => GroupedImpl
        case HoldImpl         .typeId => HoldImpl
        case IndexOfSliceImpl .typeId => IndexOfSliceImpl
        case IndicesImpl      .typeId => IndicesImpl
        case ItImpl           .typeId => ItImpl
        case LengthImpl       .typeId => LengthImpl
        case LinExpImpl       .typeId => LinExpImpl
        case LinLinImpl       .typeId => LinLinImpl
        case PatSeqImpl       .typeId => PatSeqImpl
        case PollImpl         .typeId => PollImpl
        case ShuffleImpl      .typeId => ShuffleImpl
        case SlidingImpl      .typeId => SlidingImpl
        case SortedImpl       .typeId => SortedImpl
        case StutterImpl      .typeId => StutterImpl
        case SumImpl          .typeId => SumImpl
        case TakeImpl         .typeId => TakeImpl
        case TapImpl          .typeId => TapImpl
        case Tuple2_1Impl     .typeId => Tuple2_1Impl
        case Tuple2_2Impl     .typeId => Tuple2_2Impl
        case UnaryOpImpl      .typeId => UnaryOpImpl
        case UpdatedImpl      .typeId => UpdatedImpl
        case UpdatedAllImpl   .typeId => UpdatedAllImpl
        case WhiteImpl        .typeId => WhiteImpl
        case Zip2Impl         .typeId => Zip2Impl
      }
      f.readIdentified(in, access)
    }

    def write(v: Stream[S, A], out: DataOutput): Unit = v.write(out)
  }

  def apply[S <: Base[S], A](elems: A*)(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    stream.PatSeqImpl(elems)
}
abstract class Stream[S <: Base[S], +A] extends Writable with Disposable[S#Tx] { outer =>
//  Stream.COUNT += 1

  // ---- abstract ----

  def reset()(implicit tx: S#Tx): Unit

  def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean
  def next ()(implicit ctx: Context[S], tx: S#Tx): A

  protected def typeId: Int

  protected def writeData(out: DataOutput): Unit

  // ---- impl ----

  final def write(out: DataOutput): Unit = {
    out.writeInt(typeId)
    writeData(out)
  }

  final def isEmpty (implicit ctx: Context[S], tx: S#Tx): Boolean = !hasNext
  final def nonEmpty(implicit ctx: Context[S], tx: S#Tx): Boolean = hasNext

  /** Note: consumes the stream. */
  final def toIterator(implicit ctx: Context[S], tx: S#Tx): Iterator[A] = new AbstractIterator[A] {
    def hasNext: Boolean = outer.hasNext

    def next(): A = outer.next()
  }

  /** Note: consumes the stream. */
  final def toList(implicit ctx: Context[S], tx: S#Tx): List[A] = {
    val b = List.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }

  /** Note: consumes the stream. */
  final def toVector(implicit ctx: Context[S], tx: S#Tx): Vector[A] = {
    val b = Vector.newBuilder[A]
    while (hasNext) {
      b += next()
    }
    b.result()
  }
}

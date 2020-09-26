/*
 *  StreamFormat.scala
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
package impl

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.StreamFactory
import de.sciss.serial.{DataInput, DataOutput, TFormat}

import scala.annotation.switch

object StreamFormat {
  private final val sync = new AnyRef

  @volatile private var factoryMap = Map.empty[Int, StreamFactory]

  def addFactory(f: StreamFactory): Unit = {
    val tpeId = f.typeId
    sync.synchronized {
      if (factoryMap.contains(tpeId))
        throw new IllegalArgumentException(s"Stream $tpeId was already registered ($f overrides ${factoryMap(tpeId)})")

      factoryMap += tpeId -> f
    }
  }
}
final class StreamFormat[T <: Exec[T], A]()(implicit ctx: Context[T])
  extends TFormat[T, Stream[T, A]] {

  def readT(in: DataInput)(implicit tx: T): Stream[T, A] = {
    val typeId = in.readInt()
    if (typeId == 0) null else {
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
        case EmptyImpl        .typeId => EmptyImpl
        case ExpExpImpl       .typeId => ExpExpImpl
        case ExpLinImpl       .typeId => ExpLinImpl
        case FlatMapImpl      .typeId => FlatMapImpl
        case FlattenImpl      .typeId => FlattenImpl
        case FoldLeftImpl     .typeId => FoldLeftImpl
        case FormatImpl       .typeId => FormatImpl
        case GateImpl         .typeId => GateImpl
        case GeomSeqImpl      .typeId => GeomSeqImpl
        case GroupedImpl      .typeId => GroupedImpl
        case HoldImpl         .typeId => HoldImpl
        case IndexItStream    .typeId => IndexItStream
        case IndexOfSliceImpl .typeId => IndexOfSliceImpl
        case IndicesImpl      .typeId => IndicesImpl
        case ItImpl           .typeId => ItImpl
        case LengthImpl       .typeId => LengthImpl
        case LinExpImpl       .typeId => LinExpImpl
        case LinLinImpl       .typeId => LinLinImpl
        case LoopWithIndexImpl.typeId => LoopWithIndexImpl
        case MapItStream      .typeId => MapItStream
        case MapWithIndexImpl .typeId => MapWithIndexImpl
        case ParImpl          .typeId => ParImpl
        case PatMapImpl       .typeId => PatMapImpl
        case PatSeqImpl       .typeId => PatSeqImpl
        case PollImpl         .typeId => PollImpl
        case ShuffleImpl      .typeId => ShuffleImpl
        case SlidingImpl      .typeId => SlidingImpl
        case SortedImpl       .typeId => SortedImpl
        case SortWithImpl     .typeId => SortWithImpl
        case SortWithItStream .typeId => SortWithItStream
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
        case _ =>
          StreamFormat.factoryMap.getOrElse(typeId,
            throw new IllegalArgumentException(s"Unknown stream type 0x${typeId.toHexString.toUpperCase}"))
      }
      val any: Stream[T, Any] = f.readIdentified(in)
      any.asInstanceOf[Stream[T, A]]
    }
  }

  def write(v: Stream[T, A], out: DataOutput): Unit =
    if (v == null) out.writeInt(0) else v.write(out)
}

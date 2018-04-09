package de.sciss.patterns
package impl

import de.sciss.lucre.stm.Base
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.switch

final class StreamSerializer[S <: Base[S], A]()(implicit ctx: Context[S])
  extends Serializer[S#Tx, S#Acc, Stream[S, A]] {

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
      case FlatMapImpl      .typeId => FlatMapImpl
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
      case LoopWithIndexImpl.typeId => LoopWithIndexImpl
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
    val any: Stream[S, Any] = f.readIdentified(in, access)
    any.asInstanceOf[Stream[S, A]]
  }

  def write(v: Stream[S, A], out: DataOutput): Unit = v.write(out)
}

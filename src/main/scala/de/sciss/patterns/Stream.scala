/*
 *  Stream.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.patterns.PE.Value

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

/** A UGen during graph building process is a more
  * rich thing than `RawUGen`: it implements equality
  * based on `isIndividual` status and may be omitted
  * from the final graph based on `hasSideEffect` status.
  */
trait Stream[+A <: Value] extends Product {
  // !!! WE CURRENTLY DISABLE STRUCTURAL EQUALITY
  //  // initialize this first, so that debug printing in `addUGen` can use the hash code
  //  override val hashCode: Int = if (isIndividual) super.hashCode() else scala.runtime.ScalaRunTime._hashCode(this)

  override def toString: String = inputs.mkString(s"$name(", ", ", ")")

  def name      : String

  def inputs    : Vec[Stream[_]]

  val tpe: A

  private[patterns] def iterator: Iterator[tpe.Out]

  /** Additional UGen arguments that are not of type `UGenIn`.
    * These are included to achieve correct equality
    * (also as we do not transcode unary/binary operator ids
    * into special indices)
    */
  def aux: List[Stream.Aux]

  def numInputs : Int = inputs.size
//  def numOutputs: Int

  // the full UGen spec:
  // name, inputs, aux
  override final def productPrefix: String = "UGen"
  final def productArity: Int = 3

  final def productElement(n: Int): Any = (n: @switch) match {
    case 0 => name
    case 1 => inputs
    case 2 => aux
    case _ => throw new java.lang.IndexOutOfBoundsException(n.toString)
  }

  final def canEqual(x: Any): Boolean = x.isInstanceOf[Stream[_]]

  // !!! WE CURRENTLY DISABLE STRUCTURAL EQUALITY
  //  override def equals(x: Any): Boolean = (this eq x.asInstanceOf[AnyRef]) || (!isIndividual && (x match {
  //    case u: UGen =>
  //      u.name == name && u.inputs == inputs && u.aux == aux && u.canEqual(this)
  //    case _ => false
  //  }))

//  def isIndividual : Boolean
//  def hasSideEffect: Boolean
}

object Stream {
//  object SingleOut {
//    def apply(source: Pattern /* .SingleOut */, inputs: Vec[StreamIn], aux: List[Aux] = Nil, isIndividual: Boolean = false,
//              hasSideEffect: Boolean = false)(implicit b: StreamGraph.Builder): SingleOut = {
//      val res = ??? //
////        new SingleOutImpl(source = source, inputs = inputs, aux = aux,
////          isIndividual = isIndividual, hasSideEffect = hasSideEffect)
//      b.addUGen(res)
//      res
//    }
//  }
//  /** A SingleOutUGen is a UGen which has exactly one output, and
//    * hence can directly function as input to another UGen without expansion.
//    */
//  trait SingleOut extends StreamProxy with Stream {
//    final def numOutputs    = 1
//    final def outputIndex   = 0
//    final def stream: Stream  = this
//
//    def source: Pattern // .SingleOut
//  }

//  object ZeroOut {
//    def apply(source: Pattern.ZeroOut, inputs: Vec[StreamIn], aux: List[Aux] = Nil, isIndividual: Boolean = false)
//             (implicit b: StreamGraph.Builder): ZeroOut = {
//      val res = ??? // new ZeroOutImpl(source = source, inputs = inputs, aux = aux, isIndividual = isIndividual)
//      b.addUGen(res)
//      res
//    }
//  }
//  trait ZeroOut extends Stream {
//    final def numOutputs    = 0
//    final def hasSideEffect = true  // implied by having no outputs
//
//    def source: Pattern.ZeroOut
//  }
//
//  object MultiOut {
//    def apply(source: Pattern.MultiOut, inputs: Vec[StreamIn], numOutputs: Int,
//              aux: List[Aux] = Nil, isIndividual: Boolean = false, hasSideEffect: Boolean = false)
//             (implicit b: StreamGraph.Builder): MultiOut = {
//      val res = ??? //
////        new MultiOutImpl(source = source, numOutputs = numOutputs, inputs = inputs,
////        aux = aux, isIndividual = isIndividual,
////        hasSideEffect = hasSideEffect)
//      b.addUGen(res)
//      res
//    }
//  }
//  /** A class for UGens with multiple outputs. */
//  trait MultiOut extends StreamInGroup with Stream {
//
//    final def unwrap(i: Int): StreamInLike = StreamOutProxy(this, i % numOutputs)
//
//    def outputs: Vec[StreamIn] = Vector.tabulate(numOutputs)(ch => StreamOutProxy(this, ch))
//
//    def source: Pattern.MultiOut
//
//    private[patterns] final def unbubble: StreamInLike = if (numOutputs == 1) outputs(0) else this
//  }

  object Aux {
//    final case class FileOut(peer: File) extends Aux {
//      def write(out: DataOutput): Unit = {
//        out.writeByte(0)
//        out.writeUTF(peer.path)
//      }
//    }
//
//    final case class FileIn(peer: File) extends Aux {
//      def write(out: DataOutput): Unit = {
//        out.writeByte(1)
//        out.writeUTF( peer.path)
//        out.writeLong(peer.lastModified())
//        out.writeLong(peer.length())
//      }
//    }
//
//    final case class Int(peer: scala.Int) extends Aux {
//      def write(out: DataOutput): Unit = {
//        out.writeByte(2)
//        out.writeInt(peer)
//      }
//    }
//
//    final case class String(peer: scala.Predef.String) extends Aux {
//      def write(out: DataOutput): Unit = {
//        out.writeByte(3)
//        out.writeUTF(peer)
//      }
//    }
//
//    final case class AudioFileSpec(peer: de.sciss.synth.io.AudioFileSpec) extends Aux {
//      def write(out: DataOutput): Unit = {
//        out.writeByte(10)
//        de.sciss.synth.io.AudioFileSpec.Serializer.write(peer, out)
//      }
//    }
//
//    final case class ImageFileSpec(peer: ImageFile.Spec) extends Aux {
//      def write(out: DataOutput): Unit = {
//        out.writeByte(11)
//        ImageFile.Spec.Serializer.write(peer, out)
//      }
//    }
  }
  trait Aux /* extends Writable */
}

//object StreamInLike {
//  implicit def expand(ge: PE)(implicit b: StreamGraph.Builder): StreamInLike = ge.expand
//}
//sealed trait StreamInLike[+A] extends PE[A] {
////  private[patterns] def outputs: Vec[StreamInLike]
////  private[patterns] def unbubble: StreamInLike
//
//  /** Returns the UGenInLike element of index i
//    * regarding the ungrouped representation. Note
//    * that for efficiency reasons this method will
//    * automatically wrap the index around numElements!
//    */
////  private[patterns] def unwrap(i: Int): StreamInLike
////  private[patterns] def flatOutputs: Vec[StreamIn]
//
//  // ---- GE ----
//  final private[patterns] def expand(implicit b: StreamGraph.Builder): StreamInLike[A] = this
//}

///** An element that can be used as an input to a UGen.
//  * This is after multi-channel-expansion, hence implementing
//  * classes are SingleOutUGen, UGenOutProxy, ControlOutProxy, and Constant.
//  */
//sealed trait StreamIn extends StreamInLike {
//  private[patterns] def outputs: Vec[StreamIn] = Vector(this)
//  private[patterns] final def unwrap(i: Int): StreamInLike = this
//
//  // don't bother about the index
//  private[patterns] final def flatOutputs: Vec[StreamIn] = Vector(this)
//  private[patterns] final def unbubble   : StreamInLike  = this
//}

package graph {

//  object StreamInGroup {
//    private final val emptyVal = Apply(Vector.empty)
//    def empty: StreamInGroup = emptyVal
//    def apply(xs: Vec[StreamInLike]): StreamInGroup = Apply(xs)
//
//    private final case class Apply(outputs: Vec[StreamInLike]) extends StreamInGroup {
//      override def productPrefix = "UGenInGroup"
//
//      private[patterns] def numOutputs: Int = outputs.size
//      private[patterns] def unwrap(i: Int): StreamInLike = outputs(i % outputs.size)
//      private[patterns] def unbubble: StreamInLike = this
//
//      override def toString: String = outputs.mkString("UGenInGroup(", ",", ")")
//    }
//  }
//  sealed trait StreamInGroup extends StreamInLike {
//    private[patterns] def outputs: Vec[StreamInLike]
//    private[patterns] def numOutputs: Int
//    private[patterns] final def flatOutputs: Vec[StreamIn] = outputs.flatMap(_.flatOutputs)
//  }

//  sealed trait StreamProxy extends StreamIn {
//    def stream: Stream
//    def outputIndex: Int
//  }

//  object Constant {
//    def unapply(c: Constant[_]): Option[Double] = Some(c.doubleValue)
//  }
  /** A scalar constant used as an input to a UGen. */
  sealed trait Constant[A <: Value] extends PE[A] with Stream[A] /* StreamIn */ {
//    def doubleValue : Double
//    def intValue    : Int
//    def longValue   : Long

//    def value: Any

    final def aux: List[Stream.Aux] = Nil

    final def inputs: Vec[Stream[_]] = Vector.empty

//    final def isIndividual  = false
//    final def hasSideEffect = false

    final def name: String = productPrefix

    //    def toDouble(implicit b: stream.Builder): OutD = b.add(scaladsl.Source.single(BufD(doubleValue))).out
//    def toInt   (implicit b: stream.Builder): OutI = b.add(scaladsl.Source.single(BufI(intValue   ))).out
//    def toLong  (implicit b: stream.Builder): OutL = b.add(scaladsl.Source.single(BufL(longValue  ))).out

    final private[patterns] def expand(implicit b: StreamGraph.Builder): Stream[A] = this

    final def toStream(implicit b: StreamGraph.Builder): Stream[A] = this
  }
  object ConstantI {
    final val C0  = new ConstantI(0)
    final val C1  = new ConstantI(1)
    final val Cm1 = new ConstantI(-1)
  }
  final case class ConstantI(value: Int) extends Constant[Value.Int] /* with StreamIn.IntLike */ {
    def doubleValue: Double = value.toDouble
    def intValue   : Int    = value
    def longValue  : Long   = value.toLong

    override def toString: String = value.toString

    val tpe: Value.Int = Value.Int

    private[patterns] def iterator: Iterator[Int] = Iterator.continually(value)
  }
  object ConstantD {
    final val C0  = new ConstantD(0)
    final val C1  = new ConstantD(1)
    final val Cm1 = new ConstantD(-1)
  }
  final case class ConstantD(value: Double) extends Constant[Value.Double] /* with StreamIn.DoubleLike */ {
    def doubleValue: Double = value
    def intValue   : Int    = {
      if (value.isNaN) throw new ArithmeticException("NaN cannot be translated to Int")
      val r = math.rint(value)
      if (r < Int.MinValue || r > Int.MaxValue)
        throw new ArithmeticException(s"Double $value exceeds Int range")
      r.toInt
    }
    def longValue  : Long = {
      if (value.isNaN) throw new ArithmeticException("NaN cannot be translated to Long")
      val r = math.round(value)
      r
    }

    override def toString: String = value.toString

    val tpe: Value.Double = Value.Double

    private[patterns] def iterator: Iterator[Double] = Iterator.continually(value)
  }

  ///////

  final case class ConstantIs(value: Vec[Int]) extends Constant[Value.IntSeq] {
//    def doubleValue: Double = value.toDouble
//    def intValue   : Int    = value
//    def longValue  : Long   = value.toLong

    override def toString: String = value.toString

    val tpe: Value.IntSeq = Value.IntSeq

    private[patterns] def iterator: Iterator[Vec[Int]] = Iterator.continually(value)
  }

  //  object ConstantL {
//    final val C0  = new ConstantI(0)
//    final val C1  = new ConstantI(1)
//    final val Cm1 = new ConstantI(-1)
//  }
//  final case class ConstantL(value: Long) extends Constant[Long] /* with StreamIn.LongLike */ {
//    def doubleValue: Double = value.toDouble
//    def intValue   : Int    = {
//      val res = value.toInt
//      if (res != value) throw new ArithmeticException(s"Long $value exceeds Int range")
//      res
//    }
//    def longValue: Long = value
//
//    override def toString: String = value.toString
//
//    private[patterns] def iterator: Iterator[Long] = Iterator.continually(value)
//  }

//  /** A UGenOutProxy refers to a particular output of a multi-channel UGen.
//    * A sequence of these form the representation of a multi-channel-expanded
//    * UGen.
//    */
//  final case class StreamOutProxy(stream: Stream.MultiOut, outputIndex: Int)
//    extends StreamProxy {
//
//    override def toString: String =
//      if (stream.numOutputs == 1) stream.toString else s"$stream.\\($outputIndex)"
//  }
}
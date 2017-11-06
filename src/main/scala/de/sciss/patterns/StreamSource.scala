package de.sciss.patterns

import de.sciss.patterns.graph.StreamInGroup

import scala.collection.immutable.{IndexedSeq => Vec}

object StreamSource {
  trait ZeroOut   extends StreamSource[Unit, Unit]
  trait SingleOut extends SomeOut[Any /* StreamOut */]
  trait MultiOut  extends SomeOut[Vec[Any /* StreamOut */]]

  protected sealed trait SomeOut[S] extends StreamSource[StreamInLike, S] with PE.Lazy

  def unwrap[S](source: StreamSource.SomeOut[S], args: Vec[StreamInLike])(implicit b: StreamGraph.Builder): StreamInLike = {
    var uIns    = Vector.empty: Vec[StreamIn]
    var uInsOk  = true
    var exp     = 0
    args.foreach(_.unbubble match {
      case u: StreamIn => if (uInsOk) uIns :+= u
      case g: StreamInGroup =>
        exp     = math.max(exp, g.numOutputs)
        uInsOk  = false // don't bother adding further UGenIns to uIns
    })
    if (uInsOk) {
      // aka uIns.size == args.size
      source.makeUGen(uIns)
    } else {
      // rewrap(args, exp)
      StreamInGroup(Vector.tabulate(exp)(i => unwrap(source, args.map(_.unwrap(i)))))
    }
  }

  def unwrap(source: StreamSource.ZeroOut, args: Vec[StreamInLike])(implicit b: StreamGraph.Builder): Unit = {
    var uIns    = Vector.empty: Vec[StreamIn]
    var uInsOk  = true
    var exp     = 0
    args.foreach(_.unbubble match {
      case u: StreamIn => if (uInsOk) uIns :+= u
      case g: StreamInGroup =>
        exp     = math.max(exp, g.numOutputs)
        uInsOk  = false // don't bother adding further UGenIns to uIns
    })
    if (uInsOk) {
      // aka uIns.size == args.size
      source.makeUGen(uIns)
    } else {
      // rewrap(args, exp)
      var i = 0
      while (i < exp) {
        unwrap(source, args.map(_.unwrap(i)))
        i += 1
      }
    }
  }

  /** Simple forwarder to `in.expand` that can be used to access
    * the otherwise package-private method.
    */
  def expand(in: PE)(implicit b: StreamGraph.Builder): StreamInLike = in.expand

  /** Simple forwarder to `in.outputs` that can be used to access
    * the otherwise package-private method.
    */
  def outputs(in: StreamInLike): Vec[StreamInLike] = in.outputs

  /** Simple forwarder to `in.flatOutputs` that can be used to access
    * the otherwise package-private method.
    */
  def flatOutputs(in: StreamInLike): Vec[StreamIn] = in.flatOutputs

  /** Simple forwarder to `in.unbubble` that can be used to access
    * the otherwise package-private method.
    */
  def unbubble(in: StreamInLike): StreamInLike = in.unbubble

  /** Simple forwarder to `in.unwrap` that can be used to access
    * the otherwise package-private method.
    */
  def unwrapAt(in: StreamInLike, index: Int): StreamInLike = in.unwrap(index)
}

sealed trait StreamSource[U, S] extends Lazy.Expander[U] {
  protected def makeUGen(args: Vec[StreamIn])(implicit b: StreamGraph.Builder): U

  final def name: String = productPrefix

  private[patterns] def makeStream(args: Vec[StreamIn])/* (implicit b: stream.Builder) */: S
}
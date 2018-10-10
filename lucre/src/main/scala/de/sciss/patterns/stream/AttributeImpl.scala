package de.sciss.patterns.stream

import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Obj
import de.sciss.patterns.lucre.{Context => LContext}
import de.sciss.patterns.{Context, Stream, graph}
import de.sciss.serial.{DataInput, DataOutput}

object AttributeImpl {
  final val typeId = 0x61747472 // "attr"

  def expand[S <: Base[S], A](pat: graph.Attribute[A])(implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    val id        = tx.newId()
//    val inStream  = in.expand[S]
    val _hasNext  = tx.newBooleanVar(id, true)
//    new StreamImpl[S](id = id, inStream = inStream, _hasNext = _hasNext)
    ???
  }

  def readIdentified[S <: Base[S]](in: DataInput, access: S#Acc)
                                  (implicit ctx: Context[S], tx: S#Tx): Stream[S, Any] = {
    val id        = tx.readId(in, access)
    val inStream  = Stream.read[S, graph.AudioCue](in, access)
    val _hasNext  = tx.readBooleanVar(id, in)

    new StreamImpl[S, Any](id = id, key = ???, inStream = inStream, _hasNext = _hasNext)(???)
  }


  private final class StreamImpl[S <: Base[S], A](id       : S#Id,
                                                  key      : String,
                                                  inStream : Stream[S, graph.AudioCue],
                                                  _hasNext : S#Var[Boolean]
                                                 )(
                                                 implicit val tpe: Obj.Aux[A]
  )
    extends Stream[S, A] {

    protected def typeId: Int = AttributeImpl.typeId

    protected def writeData(out: DataOutput): Unit = {
      id      .write(out)
      inStream.write(out)
      _hasNext.write(out)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id      .dispose()
      inStream.dispose()
      _hasNext.dispose()
    }

    def reset()(implicit tx: S#Tx): Unit = {
      inStream.reset()
      _hasNext() = true
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = _hasNext()

    private def advance()(implicit ctx: Context[S], tx: S#Tx): Unit = {
      val v: LContext.Attribute.Value[A] = ctx.requestInput(LContext.Attribute[A](key))
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      var res = 0
      while (inStream.hasNext) {
        inStream.next()
        res += 1
      }
      _hasNext() = false
      res
      ???
    }
  }
}

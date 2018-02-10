/*
 *  GraphObj.scala
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

package de.sciss.patterns.lucre

import java.util

import de.sciss.lucre.event.{Dummy, Event, EventLike, Targets}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.model.Change
import de.sciss.patterns.Types.Aux
import de.sciss.patterns.graph.{Constant, Pseq}
import de.sciss.patterns.{Graph, Pat, ProductWithAux, Pattern => _Pattern}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.annotation.switch
import scala.util.control.NonFatal

object GraphObj extends expr.impl.ExprTypeImpl[Graph[_], GraphObj] {
  final val typeID = 300

  protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
    extends ConstImpl[S] with GraphObj[S]

  private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
    extends VarImpl[S] with GraphObj[S]

  /** A serializer for graphs. */
  object valueSerializer extends ImmutableSerializer[Graph[_]] {
    private final val SER_VERSION = 0x5347

    // we use an identity hash map, because we do _not_
    // want to alias objects in the serialization; the input
    // is an in-memory object graph.
    private type RefMapOut = util.IdentityHashMap[Product, Integer]

    private final class RefMapIn {
      var map   = Map.empty[Int, Product]
      var count = 0
    }

    private def writeProduct(p: Product, out: DataOutput, ref: RefMapOut): Unit = {
      val id0Ref = ref.get(p)
      if (id0Ref != null) {
        out.writeByte('<')
        out.writeInt(id0Ref)
        return
      }
      out.writeByte('P')
      val aux     = p match {
        case hasAux: ProductWithAux => hasAux.aux
        case _ => Nil
      }
      val pck     = p.getClass.getPackage.getName
      val prefix  = p.productPrefix
      val name    = if (pck == "de.sciss.patterns.graph") prefix else s"$pck.$prefix"
      out.writeUTF(name)
      out.writeShort(p.productArity)
      out.writeByte(aux.size)
      p.productIterator.foreach(writeElem(_, out, ref))
      aux.foreach(Aux.write(out, _))

      val id     = ref.size() // count
      ref.put(p, id)
    }

    private def writeElemSeq(xs: Seq[Any], out: DataOutput, ref: RefMapOut): Unit = {
      out.writeByte('X')
      out.writeInt(xs.size)
      xs.foreach(writeElem(_, out, ref))
    }

    private def writeElem(e: Any, out: DataOutput, ref: RefMapOut): Unit =
      e match {
        case c: Constant[_] =>
          out.writeByte('C')
          writeElem(c.value, out, ref)
//          if (c.isDouble) {
//            out.writeByte('d')
//            out.writeDouble(c.doubleValue)
//          } else if (c.isInt) {
//            out.writeByte('i')
//            out.writeInt(c.intValue)
//          } else if (c.isLong) {
//            out.writeByte('l')
//            out.writeLong(c.longValue)
//          }
        //        case r: MaybeRate =>
        //          out.writeByte('R')
        //          out.writeByte(r.id)
        case o: Option[_] =>
          out.writeByte('O')
          out.writeBoolean(o.isDefined)
          if (o.isDefined) writeElem(o.get, out, ref)
        case xs: Seq[_] =>  // 'X'. either indexed seq or var arg (e.g. wrapped array)
          writeElemSeq(xs, out, ref)
        case p: Product =>
          writeProduct(p, out, ref) // 'P' or '<'
        case i: Int =>
          out.writeByte('I')
          out.writeInt(i)
        case s: String =>
          out.writeByte('S')
          out.writeUTF(s)
        case b: Boolean   =>
          out.writeByte('B')
          out.writeBoolean(b)
        case f: Float =>
          out.writeByte('F')
          out.writeFloat(f)
        case d: Double =>
          out.writeByte('D')
          out.writeDouble(d)
      }

    def write(v: Graph[_], out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      writeNew(v, out)
    }

    private def writeNew(v: Graph[_], out: DataOutput): Unit = {
      val ref = new RefMapOut
      writeElemSeq(v.sources, out, ref)
      writeElem   (v.out    , out, ref)
      //      val ctl = v.controlProxies
      //      out.writeByte('T')
      //      out.writeInt(ctl.size)
      //      ctl.foreach(writeProduct(_, out, ref))
    }

    // expects that 'X' byte has already been read
    private def readIdentifiedSeq(in: DataInput, ref: RefMapIn): Seq[Any] = {
      val num = in.readInt()
      Vector.fill(num)(readElem(in, ref))
    }

    // expects that 'P' byte has already been read
    private def readIdentifiedProduct(in: DataInput, ref: RefMapIn): Product = {
      val prefix    = in.readUTF()
      val arity     = in.readShort()
      val numAux    = in.readByte()
      val numElem   = arity + numAux
      val className = if (Character.isUpperCase(prefix.charAt(0))) s"de.sciss.patterns.graph.$prefix" else prefix

      val res = try {
        if (numElem == 0 && className.charAt(className.length - 1) == '$') {
          // case object
          val companion = Class.forName(s"$className").getField("MODULE$").get(null)
          companion.asInstanceOf[Product]

        } else {

          // cf. stackoverflow #3039822
          val companion = Class.forName(s"$className$$").getField("MODULE$").get(null)
          val elems = new Array[AnyRef](numElem)
          var i = 0
          while (i < arity) {
            elems(i) = readElem(in, ref).asInstanceOf[AnyRef]
            i += 1
          }
          val i1 = i + numAux
          while (i < i1) {
            elems(i) = Aux.read(in)
            i += 1
          }
          //    val m         = companion.getClass.getMethods.find(_.getName == "apply")
          //      .getOrElse(sys.error(s"No apply method found on $companion"))
          val ms = companion.getClass.getMethods
          var m = null: java.lang.reflect.Method
          var j = 0
          while (m == null && j < ms.length) {
            val mj = ms(j)
            if (mj.getName == "apply" && mj.getParameterTypes.length == numElem) m = mj
            j += 1
          }
          if (m == null) sys.error(s"No apply method found on $companion")

          m.invoke(companion, elems: _*).asInstanceOf[Product]
        }

      } catch {
        case NonFatal(e) =>
          throw new IllegalArgumentException(s"While de-serializing $prefix", e)
      }

      val id        = ref.count
      ref.map      += ((id, res))
      ref.count     = id + 1
      res
    }

    private def readElem(in: DataInput, ref: RefMapIn): Any = {
      (in.readByte(): @switch) match {
        case 'C' =>
          val value = readElem(in, ref)
          Constant(value)
//          (in.readByte(): @switch) match {
//            case 'd' => ConstantD(in.readDouble())
//            case 'i' => ConstantI(in.readInt   ())
//            case 'l' => ConstantL(in.readLong  ())
//          }
        //        case 'R' => MaybeRate(in.readByte())
        case 'O' => if (in.readBoolean()) Some(readElem(in, ref)) else None
        case 'X' => readIdentifiedSeq(in, ref)
        case 'P' => readIdentifiedProduct(in, ref)
        case '<' =>
          val id = in.readInt()
          ref.map(id)
        case 'I' => in.readInt()
        case 'S' => in.readUTF()
        case 'B' => in.readBoolean()
        case 'F' => in.readFloat()
        case 'D' => in.readDouble()
      }
    }

    def read(in: DataInput): Graph[_] = {
      val cookie  = in.readShort()
      require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
      val res2  = readNew(in)
      res2
    }

    private def readNew(in: DataInput): Graph[_] = {
      val ref         = new RefMapIn
      val b1          = in.readByte()
      require(b1 == 'X')    // expecting sequence
      val numSources  = in.readInt()
      val sources     = Vector.fill(numSources) {
        readElem(in, ref).asInstanceOf[_Pattern[_]]
      }
      val out = readElem(in, ref).asInstanceOf[Pat[_]]
      new Graph(sources, out)
    }
  }

  private final val emptyCookie = 4

  override protected def readCookie[S <: Sys[S]](in: DataInput, access: S#Acc, cookie: Byte)(implicit tx: S#Tx): Ex[S] =
    cookie match {
      case `emptyCookie` =>
        val id = tx.readID(in, access)
        new Predefined(id, cookie)
      case _ => super.readCookie(in, access, cookie)
    }

  private val emptyGraph = Graph {
    Pseq(Nil) // (Vector.empty, Pseq[Top](Nil))
  }

  def empty[S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(emptyCookie)

  private def apply[S <: Sys[S]](cookie: Int)(implicit tx: S#Tx): Ex[S] = {
    val id = tx.newID()
    new Predefined(id, cookie)
  }

  private final class Predefined[S <: Sys[S]](val id: S#ID, cookie: Int)
    extends GraphObj[S] with Expr.Const[S, Graph[_]] {

    def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

    def tpe: Obj.Type = GraphObj

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Predefined(txOut.newID(), cookie) // .connect()

    def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeID)
      out.writeByte(cookie)
      id.write(out)
    }

    def value(implicit tx: S#Tx): Graph[_] = constValue

    def changed: EventLike[S, Change[Graph[_]]] = Dummy[S, Change[Graph[_]]]

    def dispose()(implicit tx: S#Tx): Unit = ()

    def constValue: Graph[_] = cookie match {
      case `emptyCookie` => emptyGraph
    }
  }
}
trait GraphObj[S <: Sys[S]] extends Expr[S, Graph[_]]
/*
 *  PatElem.scala
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
package impl

import java.util

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.Aux
import de.sciss.patterns.graph.Constant
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.annotation.switch
import scala.util.control.NonFatal

object PatElem {
  private type RefMapOut = util.IdentityHashMap[Product, Integer]

  private final class RefMapIn {
    var map   = Map.empty[Int, Product]
    var count = 0
  }

  def makeVar[S <: Base[S], A](id: S#Id)(implicit tx: S#Tx): S#Var[A] =
    tx.newVar[A](id, null.asInstanceOf[A])(serializer)

  def read[A](in: DataInput): A = read(in, null)

  private def read[A](in: DataInput, ref0: RefMapIn): A = {
    val res: Any = (in.readByte(): @switch) match {
      case 'C' =>
        val value = read(in, ref0)
        Constant(value)
      case 'O' => if (in.readBoolean()) {
        val ref = if (ref0 == null) new RefMapIn else ref0
        Some(read(in, ref))
      } else None
      case 'X' =>
        val ref = if (ref0 == null) new RefMapIn else ref0
        readIdentifiedSeq(in, ref)
      case 'P' =>
        val ref = if (ref0 == null) new RefMapIn else ref0
        readIdentifiedProduct(in, ref)
      case '<' =>
        val id = in.readInt()
        ref0.map(id)    // at this point ref0 must be non-null or we have a bug
      case 'I' => in.readInt()
      case 'S' => in.readUTF()
      case 'B' => in.readBoolean()
      case 'F' => in.readFloat()
      case 'D' => in.readDouble()
      case 'L' => in.readLong()
    }
    res.asInstanceOf[A]
  }

  // expects that 'X' byte has already been read
  private def readIdentifiedSeq(in: DataInput, ref: RefMapIn): Seq[Any] = {
    val num = in.readInt()
    Vector.fill(num)(read(in, ref))
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
          elems(i) = read(in, ref).asInstanceOf[AnyRef]
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
          if (mj.getName == "apply") {
            if (mj.getParameterCount == numElem) {
              val types = mj.getParameterTypes
              // actually check the types to deal with overloaded `apply` method.
              var k = 0
              while (k < types.length) {
                val tpe = types(k)
                // XXX TODO --- cheesy shortcut for https://stackoverflow.com/questions/7082997/
                if (tpe.isPrimitive || tpe.isAssignableFrom(elems(k).getClass)) k += 1
                else {
                  k = Int.MaxValue
                }
              }
              if (k == types.length) m = mj
            }
          }
          j += 1
        }
        if (m == null) {
          sys.error(s"No apply method found on $companion")
        }

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

  def write[A](v: A, out: DataOutput): Unit = write(v, out, null)

  private def write[A](v: A, out: DataOutput, ref0: RefMapOut): Unit = v match {
    case c: Constant[_] =>
      out.writeByte('C')
      write(c.value, out, ref0)
    case o: Option[_] =>
      out.writeByte('O')
      out.writeBoolean(o.isDefined)
      if (o.isDefined) {
        val ref = if (ref0 == null) new RefMapOut else ref0
        write(o.get, out, ref)
      }
    case xs: Seq[_] =>  // 'X'. either indexed seq or var arg (e.g. wrapped array)
      val ref = if (ref0 == null) new RefMapOut else ref0
      writeSeq(xs, out, ref)
    case p: Product =>
      val ref = if (ref0 == null) new RefMapOut else ref0
      writeProduct(p, out, ref) // 'P' or '<'
    case i: Int =>
      out.writeByte('I')
      out.writeInt(i)
    case s: String =>
      out.writeByte('S')
      out.writeUTF(s)
    case b: Boolean =>
      out.writeByte('B')
      out.writeBoolean(b)
    case f: Float =>
      out.writeByte('F')
      out.writeFloat(f)
    case d: Double =>
      out.writeByte('D')
      out.writeDouble(d)
    case l: Long =>
      out.writeByte('L')
      out.writeLong(l)
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
    p.productIterator.foreach(write(_, out, ref))
    aux.foreach(Aux.write(out, _))

    val id      = ref.size() // count
    ref.put(p, id)
  }

  private def writeSeq(xs: Seq[Any], out: DataOutput, ref: RefMapOut): Unit = {
    out.writeByte('X')
    out.writeInt(xs.size)
    xs.foreach(write(_, out, ref))
  }

  implicit def serializer[A]: ImmutableSerializer[A] = Ser.asInstanceOf[ImmutableSerializer[A]]

  private object Ser extends ImmutableSerializer[Any] {
    def write(v: Any, out: DataOutput): Unit  = PatElem.write(v, out)
    def read          (in: DataInput ): Any   = PatElem.read(in)
  }
}

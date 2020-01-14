/*
 *  PatElem.scala
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

import java.util

import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.stm.Base
import de.sciss.patterns.graph.Constant
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable
import scala.util.control.NonFatal

object PatElem {
  type RefMapOut = util.IdentityHashMap[Product, Integer]

  final class RefMapIn {
    var map   = Map.empty[Int, Product]
    var count = 0
  }

  def makeVar[S <: Base[S], A](id: S#Id)(implicit tx: S#Tx): S#Var[A] =
    tx.newVar[A](id, null.asInstanceOf[A])(serializer)

  def readVar[S <: Base[S], A](id: S#Id, in: DataInput)(implicit tx: S#Tx): S#Var[A] =
    tx.readVar[A](id, in)(serializer)

  def read[A](in: DataInput): A = read(in, null).asInstanceOf[A]

  def read(in: DataInput, ref0: RefMapIn): Any =
    (in.readByte(): @switch) match {
      case 'C' =>
        val value = read(in, ref0)
        Constant(value)
      case 'O' => if (in.readBoolean()) {
        val ref = if (ref0 == null) new RefMapIn else ref0
        Some(read(in, ref))
      } else None
      case 'X' =>
        val ref = if (ref0 == null) new RefMapIn else ref0
        val num = in.readInt()
        val b   = Vector.newBuilder[Any]
        b.sizeHint(num)
        var rem = num
        while (rem > 0) {
          b += read(in, ref)
          rem -= 1
        }
        b.result()
      case 'E' =>
        val ref = if (ref0 == null) new RefMapIn else ref0
        val num = in.readInt()
        val b   = Map.newBuilder[String, Any]
        b.sizeHint(num)
        var rem = num
        while (rem > 0) {
          val k = in.readUTF()
          val v = read(in, ref)
          b += k -> v
          rem -= 1
        }
        val m = b.result()
        Event(m)
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
      case '\u0000' => null
    }

  // expects that 'P' byte has already been read
  private def readIdentifiedProduct(in: DataInput, ref: RefMapIn): Product = {
    val prefix    = in.readUTF()
    val arity     = in.readShort()
    val numAdj    = in.readByte()
    val numElem   = arity + numAdj
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
        val i1 = i + numAdj
        while (i < i1) {
          elems(i) = Adjunct.read(in)
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

  def write(v: Any, out: DataOutput, ref0: RefMapOut): RefMapOut = v match {
    case c: Constant[_] =>
      out.writeByte('C')
      write(c.value, out, ref0)

    case o: Option[_] =>
      out.writeByte('O')
      out.writeBoolean(o.isDefined)
      if (o.isEmpty) ref0 else {
        val ref = if (ref0 == null) new RefMapOut else ref0
        write(o.get, out, ref)
      }

    case xs: Iterable[_] =>  // 'X'. either indexed seq or var arg (e.g. wrapped array)
      var ref = if (ref0 == null) new RefMapOut else ref0
      xs match {
        case _: Seq[_] =>
          out.writeByte('X')
          out.writeInt(xs.size)
          xs.foreach(x => ref = write(x, out, ref))
        case ev: Event =>
          val m = ev.map
          out.writeByte('E')
          out.writeInt(m.size)
          m.foreach { tup =>
            out.writeUTF(tup._1)
            ref = write(tup._2, out, ref)
          }
        case _ => throw new Exception(s"Unsupported collection $xs")
      }
      ref

    case p: Product =>
      val ref = if (ref0 == null) new RefMapOut else ref0
      writeProduct(p, out, ref) // 'P' or '<'

    case i: Int =>
      out.writeByte('I')
      out.writeInt(i)
      ref0

    case s: String =>
      out.writeByte('S')
      out.writeUTF(s)
      ref0

    case b: Boolean =>
      out.writeByte('B')
      out.writeBoolean(b)
      ref0

    case f: Float =>
      out.writeByte('F')
      out.writeFloat(f)
      ref0

    case d: Double =>
      out.writeByte('D')
      out.writeDouble(d)
      ref0

    case l: Long =>
      out.writeByte('L')
      out.writeLong(l)
      ref0

    case null =>
      out.writeByte('\u0000')
      ref0
  }

  private def writeProduct(p: Product, out: DataOutput, ref0: RefMapOut): RefMapOut = {
    val id0Ref = ref0.get(p)
    if (id0Ref != null) {
      out.writeByte('<')
      out.writeInt(id0Ref)
      return ref0
    }
    out.writeByte('P')
    val adjuncts = p match {
      case hasAdj: ProductWithAdjuncts => hasAdj.adjuncts
      case _ => Nil
    }
    val pck     = p.getClass.getPackage.getName
    val prefix  = p.productPrefix
    val name    = if (pck == "de.sciss.patterns.graph") prefix else s"$pck.$prefix"
    out.writeUTF(name)
    out.writeShort(p.productArity)
    out.writeByte(adjuncts.size)

    var ref = ref0
    val it = p.productIterator
    while (it.hasNext) {
      ref = write(it.next(), out, ref)
    }
    adjuncts.foreach(Adjunct.write(out, _))

    val id      = ref.size() // count
    ref.put(p, id)
    ref
  }

  implicit def serializer   [A]: ImmutableSerializer[A]       = Ser   .asInstanceOf[ImmutableSerializer[A]]
  implicit def vecSerializer[A]: ImmutableSerializer[Vec[A]]  = VecSer.asInstanceOf[ImmutableSerializer[Vec[A]]]
  implicit def setSerializer[A]: ImmutableSerializer[Set[A]]  = SetSer.asInstanceOf[ImmutableSerializer[Set[A]]]

  private object Ser extends ImmutableSerializer[Any] {
    def write(v: Any, out: DataOutput): Unit  = PatElem.write(v, out)
    def read          (in: DataInput ): Any   = PatElem.read(in)
  }

  private abstract class CollectionSer[That <: Iterable[Any]] extends ImmutableSerializer[That] {
    def newBuilder: mutable.Builder[Any, That]
    def empty: That

    def read(in: DataInput): That = {
      var sz  = in.readInt()
      if (sz == 0) empty else {
        val b   = newBuilder
        b.sizeHint(sz)
        val ref: RefMapIn = if (sz == 1) null else new RefMapIn
        while (sz > 0) {
          b += PatElem.read(in, ref)
          sz -= 1
        }
        b.result()
      }
    }

    def write(coll: That, out: DataOutput): Unit = {
      val sz = coll.size
      out.writeInt(sz)
      if (sz > 0) {
        var ref: RefMapOut = null
        coll.foreach{ x =>
          ref = PatElem.write(x, out, ref)
        }
      }
    }
  }

  private object VecSer extends CollectionSer[Vec[Any]] {
    def newBuilder: mutable.Builder[Any, Vec[Any]] = Vector.newBuilder
    def empty: Vec[Any] = Vector.empty
  }

  private object SetSer extends CollectionSer[Set[Any]] {
    def newBuilder: mutable.Builder[Any, Set[Any]] = Set.newBuilder
    def empty: Set[Any] = Set.empty
  }
}

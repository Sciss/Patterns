/*
 *  Obj.scala
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

import de.sciss.lucre.Adjunct.Factory
import de.sciss.lucre.{BooleanObj, DoubleObj, IntObj, LongObj, StringObj, Txn, Adjunct => _Adjunct, Obj => LObj}
import de.sciss.patterns.graph.{AudioCue, Folder}
import de.sciss.serial.DataInput

object Obj {
  private lazy val _init: Unit = {
    _Adjunct.addFactory(Extractor.Int)
    _Adjunct.addFactory(Extractor.Long)
    _Adjunct.addFactory(Extractor.Double)
    _Adjunct.addFactory(Extractor.Boolean)
    _Adjunct.addFactory(Extractor.String)
    _Adjunct.addFactory(AudioCue )
    _Adjunct.addFactory(Folder   )
  }

  def init(): Unit = _init

  object Extractor {
    implicit object Int extends Adjunct[scala.Int] with Factory {
      final val id = 0x400

      def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[scala.Int] = obj match {
        case i: IntObj    [T] => Some(i.value)
        case d: DoubleObj [T] => Some(d.value.toInt)
        case b: BooleanObj[T] => Some(if (b.value) 1 else 0)
        case n: LongObj   [T] => Some(n.value.toInt)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object Double extends Adjunct[scala.Double] with Factory {
      final val id = 0x402

      def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[scala.Double] = obj match {
        case d: DoubleObj [T] => Some(d.value)
        case i: IntObj    [T] => Some(i.value.toDouble)
        case b: BooleanObj[T] => Some(if (b.value) 1.0 else 0.0)
        case n: LongObj   [T] => Some(n.value.toDouble)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object Boolean extends Adjunct[scala.Boolean] with Factory {
      final val id = 0x404

      def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[scala.Boolean] = obj match {
        case b: BooleanObj[T] => Some(b.value)
        case i: IntObj    [T] => Some(i.value != 0)
        case d: DoubleObj [T] => Some(d.value != 0.0)
        case n: LongObj   [T] => Some(n.value != 0L)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object Long extends Adjunct[scala.Long] with Factory {
      final val id = 0x406

      def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[scala.Long] = obj match {
        case n: LongObj   [T] => Some(n.value)
        case i: IntObj    [T] => Some(i.value.toLong)
        case d: DoubleObj [T] => Some(d.value.toLong)
        case b: BooleanObj[T] => Some(if (b.value) 1L else 0L)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object String extends Adjunct[java.lang.String] with Factory {
      final val id = 0x410

      def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[java.lang.String] = obj match {
        case s: StringObj [T] => Some(s.value)
        case i: IntObj    [T] => Some(i.value.toString)
        case d: DoubleObj [T] => Some(d.value.toString)
        case b: BooleanObj[T] => Some(b.value.toString)
        case n: LongObj   [T] => Some(n.value.toString)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }
  }

  // XXX TODO --- DRY --- there should be an equivalent in Lucre now?
  trait Extractor[+A] {
//    def typeId: Int

//    type Repr[T <: Txn[T]] <: LObj[T]

    def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[A]
  }

  trait Adjunct[A] extends Extractor[A] with _Adjunct
}

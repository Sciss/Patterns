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

import de.sciss.lucre.adjunct.Adjunct.Factory
import de.sciss.lucre.adjunct.{Adjunct => _Adjunct}
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
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

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Int] = obj match {
        case i: IntObj    [S] => Some(i.value)
        case d: DoubleObj [S] => Some(d.value.toInt)
        case b: BooleanObj[S] => Some(if (b.value) 1 else 0)
        case n: LongObj   [S] => Some(n.value.toInt)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object Double extends Adjunct[scala.Double] with Factory {
      final val id = 0x402

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Double] = obj match {
        case d: DoubleObj [S] => Some(d.value)
        case i: IntObj    [S] => Some(i.value.toDouble)
        case b: BooleanObj[S] => Some(if (b.value) 1.0 else 0.0)
        case n: LongObj   [S] => Some(n.value.toDouble)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object Boolean extends Adjunct[scala.Boolean] with Factory {
      final val id = 0x404

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Boolean] = obj match {
        case b: BooleanObj[S] => Some(b.value)
        case i: IntObj    [S] => Some(i.value != 0)
        case d: DoubleObj [S] => Some(d.value != 0.0)
        case n: LongObj   [S] => Some(n.value != 0L)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object Long extends Adjunct[scala.Long] with Factory {
      final val id = 0x406

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Long] = obj match {
        case n: LongObj   [S] => Some(n.value)
        case i: IntObj    [S] => Some(i.value.toLong)
        case d: DoubleObj [S] => Some(d.value.toLong)
        case b: BooleanObj[S] => Some(if (b.value) 1L else 0L)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }

    implicit object String extends Adjunct[java.lang.String] with Factory {
      final val id = 0x410

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[java.lang.String] = obj match {
        case s: StringObj [S] => Some(s.value)
        case i: IntObj    [S] => Some(i.value.toString)
        case d: DoubleObj [S] => Some(d.value.toString)
        case b: BooleanObj[S] => Some(b.value.toString)
        case n: LongObj   [S] => Some(n.value.toString)
        case _                => None
      }

      override def readIdentifiedAdjunct(in: DataInput): _Adjunct = this
    }
  }

  // XXX TODO --- DRY --- there should be an equivalent in Lucre now?
  trait Extractor[+A] {
//    def typeId: Int

//    type Repr[S <: Sys[S]] <: stm.Obj[S]

    def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[A]
  }

  trait Adjunct[A] extends Extractor[A] with _Adjunct
}

package de.sciss.patterns

import de.sciss.lucre.aux.Aux.Factory
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj, StringObj}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{aux, stm}
import de.sciss.patterns.graph.{AudioCue, Folder}
import de.sciss.serial.DataInput

object Obj {
  private lazy val _init: Unit = {
    aux.Aux.addFactory(Extractor.Int)
    aux.Aux.addFactory(Extractor.Long)
    aux.Aux.addFactory(Extractor.Double)
    aux.Aux.addFactory(Extractor.Boolean)
    aux.Aux.addFactory(Extractor.String)
    aux.Aux.addFactory(AudioCue )
    aux.Aux.addFactory(Folder   )
  }

  def init(): Unit = _init

  object Extractor {
    implicit object Int extends Aux[scala.Int] with Factory {
      final val id = 0x400

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Int] = obj match {
        case i: IntObj    [S] => Some(i.value)
        case d: DoubleObj [S] => Some(d.value.toInt)
        case b: BooleanObj[S] => Some(if (b.value) 1 else 0)
        case n: LongObj   [S] => Some(n.value.toInt)
        case _                => None
      }

      def readIdentifiedAux(in: DataInput): aux.Aux = this
    }

    implicit object Double extends Aux[scala.Double] with Factory {
      final val id = 0x402

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Double] = obj match {
        case d: DoubleObj [S] => Some(d.value)
        case i: IntObj    [S] => Some(i.value.toDouble)
        case b: BooleanObj[S] => Some(if (b.value) 1.0 else 0.0)
        case n: LongObj   [S] => Some(n.value.toDouble)
        case _                => None
      }

      def readIdentifiedAux(in: DataInput): aux.Aux = this
    }

    implicit object Boolean extends Aux[scala.Boolean] with Factory {
      final val id = 0x404

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Boolean] = obj match {
        case b: BooleanObj[S] => Some(b.value)
        case i: IntObj    [S] => Some(i.value != 0)
        case d: DoubleObj [S] => Some(d.value != 0.0)
        case n: LongObj   [S] => Some(n.value != 0L)
        case _                => None
      }

      def readIdentifiedAux(in: DataInput): aux.Aux = this
    }

    implicit object Long extends Aux[scala.Long] with Factory {
      final val id = 0x406

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Long] = obj match {
        case n: LongObj   [S] => Some(n.value)
        case i: IntObj    [S] => Some(i.value.toLong)
        case d: DoubleObj [S] => Some(d.value.toLong)
        case b: BooleanObj[S] => Some(if (b.value) 1L else 0L)
        case _                => None
      }

      def readIdentifiedAux(in: DataInput): aux.Aux = this
    }

    implicit object String extends Aux[java.lang.String] with Factory {
      final val id = 0x410

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[java.lang.String] = obj match {
        case s: StringObj [S] => Some(s.value)
        case i: IntObj    [S] => Some(i.value.toString)
        case d: DoubleObj [S] => Some(d.value.toString)
        case b: BooleanObj[S] => Some(b.value.toString)
        case n: LongObj   [S] => Some(n.value.toString)
        case _                => None
      }

      def readIdentifiedAux(in: DataInput): aux.Aux = this
    }
  }

  trait Extractor[+A] {
//    def typeId: Int

//    type Repr[S <: Sys[S]] <: stm.Obj[S]

    def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[A]
  }

  trait Aux[A] extends Extractor[A] with aux.Aux
}

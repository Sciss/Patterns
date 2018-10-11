/*
 *  Obj.scala
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
package graph

import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj}
import de.sciss.lucre.{aux, stm}
import de.sciss.lucre.stm.Sys

object Obj {
  object Extractor {
    implicit object Int extends Aux[scala.Int] {
//      def typeId: Int = IntObj.typeId

      // type Repr[S <: Sys[S]] = IntObj[S]

      final val id = 0x200

      def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Int] = obj match {
        case i: IntObj    [S] => Some(i.value)
        case d: DoubleObj [S] => Some(d.value.toInt)
        case b: BooleanObj[S] => Some(if (b.value) 1 else 0)
        case n: LongObj   [S] => Some(n.value.toInt)
        case _                => None
      }
    }
  }

  trait Extractor[+A] {
//    def typeId: Int

//    type Repr[S <: Sys[S]] <: stm.Obj[S]

    def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[A]
  }

  trait Aux[A] extends Extractor[A] with aux.Aux
}
//trait Obj {
//
//}

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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys

import scala.language.higherKinds

object Obj {
  object Type {
    implicit object Int extends Type[scala.Int] {
      def typeId: Int = IntObj.typeId

      // type Repr[S <: Sys[S]] = IntObj[S]

      def translate[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[scala.Int] = obj match {
        case i: IntObj    [S] => Some(i.value)
        case d: DoubleObj [S] => Some(d.value.toInt)
        case b: BooleanObj[S] => Some(if (b.value) 1 else 0)
        case n: LongObj   [S] => Some(n.value.toInt)
        case _                => None
      }
    }
  }

  trait Type[A] {
    def typeId: Int

//    type Repr[S <: Sys[S]] <: stm.Obj[S]

    def translate[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[A]
  }
}
//trait Obj {
//
//}

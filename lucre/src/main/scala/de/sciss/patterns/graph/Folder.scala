/*
 *  Folder.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.adjunct.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.patterns.stream.FolderCollectImpl
import de.sciss.serial.DataInput

object Folder extends Obj.Adjunct[Folder] with Adjunct.Factory {
//  def typeId: Int = stm.Folder.typeId

  final val id = 0x480

  //  type Repr[S <: Sys[S]] = stm.Folder[S]

  implicit def tpe: Obj.Adjunct[Folder] = this

  override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

  def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[Folder] = obj match {
    case _: stm.Folder[S] => Some(Folder())
    case _ => None
  }

  final case class Collect[A](key: String)(implicit val ex: Obj.Adjunct[A])
    extends Pattern[A] with ProductWithAdjuncts {

    override def adjuncts: List[Adjunct] = ex :: Nil

    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
      FolderCollectImpl.expand(this)

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
      this
//      val inT = t(in)
//      if (inT.eq(in)) this else copy(in = inT)
    }

    override def productPrefix: String = s"Folder$$Collect"
  }
}
final case class Folder() /* (key: Pat[String]) */ /* extends Pattern[Obj] */ /* with Obj */ {
//  def collect[A](implicit tpe: Obj.Type[A]): Pat[A] =
//    Folder.Collect[A](this)

//  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Obj] = ...
//
//  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Obj] = {
//    val keyT = t(key)
//    if (keyT.eq(key)) this else copy(key = keyT)
//  }
}
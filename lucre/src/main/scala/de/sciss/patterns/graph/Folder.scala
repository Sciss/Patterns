/*
 *  Folder.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Base, Sys}
import de.sciss.patterns.stream.FolderCollectImpl

object Folder extends Obj.Aux[Folder] {
//  def typeId: Int = stm.Folder.typeId

  final val id = 0x280

  //  type Repr[S <: Sys[S]] = stm.Folder[S]

  implicit def tpe: Obj.Aux[Folder] = this

//  def translate[S <: Sys[S]](obj: stm.Folder[S])(implicit tx: S#Tx): Folder = ...


  def extract[S <: Sys[S]](obj: stm.Obj[S])(implicit tx: S#Tx): Option[Folder] = ???

  final case class Collect[A](in: Pat[Folder]) extends Pattern[Pat[A]] {
    def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] =
      FolderCollectImpl.expand(this)

    def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
      ???
//      val folderT = t(folder)
//      if (folderT.eq(folder)) this else copy(folder = folderT.asInstanceOf[Folder]) // XXX TODO: ugly
    }

    override def productPrefix: String = s"Folder$$Collect"
  }
}
final case class Folder() /* (key: Pat[String]) */ /* extends Pattern[Obj] */ /* with Obj */ {
//  def collect[A](implicit tpe: Obj.Type[A]): Pat[A] =
//    Folder.Collect[A](this)

//  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Obj] = ???
//
//  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Obj] = {
//    val keyT = t(key)
//    if (keyT.eq(key)) this else copy(key = keyT)
//  }
}
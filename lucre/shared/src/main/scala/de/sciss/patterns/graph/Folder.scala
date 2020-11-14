/*
 *  Folder.scala
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
package graph

import de.sciss.lucre.{Adjunct, Exec, ProductWithAdjuncts, Txn, Folder => LFolder, Obj => LObj}
import de.sciss.patterns.stream.FolderCollectImpl
import de.sciss.serial.DataInput

object Folder extends Obj.Adjunct[Folder] with Adjunct.Factory {
//  def typeId: Int = stm.Folder.typeId

  final val id = 0x480

  //  type Repr[T <: Txn[T]] = stm.Folder[T]

  implicit def tpe: Obj.Adjunct[Folder] = this

  override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

  def extract[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Folder] = obj match {
    case _: LFolder[T] => Some(Folder())
    case _ => None
  }

  final case class Collect[A](key: String)(implicit val ex: Obj.Adjunct[A])
    extends Pattern[A] with ProductWithAdjuncts {

    override def adjuncts: List[Adjunct] = ex :: Nil

    def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
      FolderCollectImpl.expand(this)

    def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
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

//  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Obj] = ...
//
//  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Obj] = {
//    val keyT = t(key)
//    if (keyT.eq(key)) this else copy(key = keyT)
//  }
}
/*
 *  Attribute.scala
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

import de.sciss.lucre.{Adjunct, Exec, ProductWithAdjuncts}
import de.sciss.patterns.stream.AttributeImpl

object Attribute {
  final class Factory(val `this`: String) extends AnyVal { me =>
    import me.{`this` => name}

    /** Creates an attribute without defaults (attribute must be present). */
    def attr[A: Obj.Adjunct]: Attribute[A] = Attribute(key = name, default = None)
    /** Creates an attribute with defaults (attribute may be absent). */
    def attr[A: Obj.Adjunct](default: Pat[A]): Attribute[A] = Attribute(key = name, default = Some(default))
  }
}
final case class Attribute[A](key: String, default: Option[Pat[A]])(implicit val ex: Obj.Adjunct[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = ex :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    AttributeImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] =
    default.fold(this) { defaultV =>
      val defaultVT = t(defaultV)
      if (defaultVT.eq(defaultV)) this else copy(key = key, default = Some(defaultVT))
    }
}
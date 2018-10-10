/*
 *  Attribute.scala
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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.AttributeImpl

object Attribute {
  final class Factory(val `this`: String) extends AnyVal { me =>
    import me.{`this` => name}

    /** Creates an attribute without defaults (attribute must be present). */
    def attr[A: Obj.Type]: Attribute[A] = Attribute(key = name, default = None)
    /** Creates an attribute with defaults (attribute may be absent). */
    def attr[A: Obj.Type](default: Pat[A]): Attribute[A] = Attribute(key = name, default = Some(default))
  }
}
final case class Attribute[A](key: String, default: Option[Pat[A]]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    AttributeImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] =
    default.fold(this) { defaultV =>
      val defaultVT = t(defaultV)
      if (defaultVT.eq(defaultV)) this else copy(key = key, default = Some(defaultVT))
    }
}
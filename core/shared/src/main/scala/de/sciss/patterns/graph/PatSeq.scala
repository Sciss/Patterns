/*
 *  PatSeq.scala
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

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.PatSeqImpl

final case class PatSeq[A](elems: A*) extends Pattern[A] {
  private def simpleString: String = {
    val xs = elems.iterator.take(5).toList
    val es = if (xs.lengthCompare(5) == 0) xs.init.mkString("", ", ", ", ...")
      else xs.mkString(", ")
    s"Pat($es)"
  }

  override def toString: String = simpleString

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    PatSeqImpl(elems)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val elemsT: Seq[_] = elems.map {
      case e: Pat[_]  => t(e)
      case e          => e
    }
    val elemsC = elemsT.asInstanceOf[Seq[A]]
    PatSeq(elemsC: _*)
  }
}

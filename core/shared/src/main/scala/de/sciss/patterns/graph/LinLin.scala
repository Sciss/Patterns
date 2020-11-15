/*
 *  LinLin.scala
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

package de.sciss.patterns.graph

import de.sciss.lucre.Adjunct.{NumFrac, Widen2}
import de.sciss.lucre.{Adjunct, ProductWithAdjuncts}
import de.sciss.lucre.Exec
import de.sciss.patterns.stream.LinLinImpl
import de.sciss.patterns.{Context, Stream, Transform}

final case class LinLin[A1, A2, A](in: Pat[A1], inLo: Pat[A1], inHi: Pat[A1],
                                   outLo: Pat[A2], outHi: Pat[A2])
                                  (implicit val widen: Widen2[A1, A2, A], val num: NumFrac[A])
  extends Pattern[A] with ProductWithAdjuncts {

  override def adjuncts: List[Adjunct] = widen :: num :: Nil

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    LinLinImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT     = t(in)
    val inLoT   = t(inLo)
    val inHiT   = t(inHi)
    val outLoT  = t(outLo)
    val outHiT  = t(outHi)
    if (inT.eq(in) && inLoT.eq(inLo) && inHiT.eq(inHi) && outLoT.eq(outLo) && outHiT.eq(outHi)) this
    else copy(in = inT, inLo = inLoT, inHi = inHiT, outLo = outLoT, outHi = outHiT)
  }
}

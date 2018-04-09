package de.sciss.patterns
package graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.{Aux, Widen2}
import de.sciss.patterns.stream.CatImpl

final case class Cat[A1, A2, A](a: Pat[A1], b: Pat[A2])
                               (implicit val widen: Widen2[A1, A2, A])
  extends Pattern[A] {

  override private[patterns] def aux: List[Aux] = widen :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    CatImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val aT = t(a)
    val bT = t(b)
    if (aT.eq(a) && bT.eq(b)) this else copy(a = aT, b = bT)
  }
}
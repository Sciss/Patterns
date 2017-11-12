package de.sciss.patterns
package graph

import de.sciss.patterns.Types.Top

final case class Ppar[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[T] {

  def iterator(implicit ctx: Context): Iterator[T#Out] = ???
}

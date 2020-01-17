/*
 *  Macros.scala
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

package de.sciss.patterns.lucre
package impl

import de.sciss.patterns.{Graph, Pat}
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Code
import de.sciss.synth.proc.impl.Macros.mkSource

import scala.reflect.macros.blackbox

object Macros {
  def patternGraphWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[Pat[_]])(tx: c.Expr[S#Tx])
                                         (implicit tt: c.WeakTypeTag[S]): c.Expr[Unit] = { // yes, `tt` _is_ used
    import c.universe._

    val source      = mkSource(c)("pattern", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    reify {
      val ext           = c.prefix.splice.asInstanceOf[MacroImplicits.PatternMacroOps[S]]
      implicit val txc  = tx.splice // N.B.: don't annotate the type with `S#Tx`, it will break scalac
      val p             = ext.`this`
      p()               = Pattern.newConst[S](Graph(body.splice))
      val code          = Pattern.Code(sourceExpr.splice)
      val codeObj       = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
      p.attr.put(Pattern.attrSource, codeObj)
    }
  }
}
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

// fragile -- don't touch!
object Macros {
  def patternGraphWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[Pat[Any]])(tx: c.Expr[S#Tx])
                                         (implicit tt: c.WeakTypeTag[S]): c.Expr[Unit] = { // yes, `tt` _is_ used
    import c.universe._

    val source      = mkSource(c)("pattern", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    reify {
      implicit val txc  = tx.splice // N.B.: don't annotate the type with `S#Tx`, it will break scalac
      val ext           = c.prefix.splice.asInstanceOf[MacroImplicits.PatternMacroOps[S]]
      val obj           = ext.`this`
      obj()             = Pattern.newConst[S](Graph(body.splice))  // careful, intermediate variable would break scalac
      val code          = Pattern.Code(sourceExpr.splice)
      val codeObj       = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
      obj.attr.put(Pattern.attrSource, codeObj)
    }
  }

  // XXX TODO --- DRY
  def streamGraphWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[Pat[Any]])(tx: c.Expr[S#Tx])
                                        (implicit tt: c.WeakTypeTag[S]): c.Expr[Unit] = { // yes, `tt` _is_ used
    import c.universe._

    val source      = mkSource(c)("stream", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    reify {
      implicit val txc  = tx.splice // N.B.: don't annotate the type with `S#Tx`, it will break scalac
      val ext           = c.prefix.splice.asInstanceOf[MacroImplicits.StreamMacroOps[S]]
      val obj           = ext.`this`
      import obj.context
      obj.peer()        = Graph(body.splice).expand[S]  // careful, intermediate variable would break scalac
      val code          = Pattern.Code(sourceExpr.splice)
      val codeObj       = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
      obj.attr.put(Stream.attrSource, codeObj)
    }
  }
}
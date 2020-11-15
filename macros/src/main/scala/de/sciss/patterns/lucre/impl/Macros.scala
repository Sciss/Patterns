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

import de.sciss.patterns.Graph
import de.sciss.lucre.Txn
import de.sciss.patterns.graph.Pat
import de.sciss.proc.{Code, Pattern}
import de.sciss.proc.impl.Macros.mkSource

import scala.reflect.macros.blackbox

// fragile -- don't touch!
object Macros {
  def patternGraphWithSource[T <: Txn[T]](c: blackbox.Context)(body: c.Expr[Pat[Any]])(tx: c.Expr[T])
                                         (implicit tt: c.WeakTypeTag[T]): c.Expr[Unit] = { // yes, `tt` _is_ used
    import c.universe._

    val source      = mkSource(c)("pattern", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    reify {
      implicit val txc  = tx.splice // N.B.: don't annotate the type with `S#Tx`, it will break scalac
      val ext           = c.prefix.splice.asInstanceOf[MacroImplicits.PatternMacroOps[T]]
      val obj           = ext.`this`
      obj()             = Pattern.newConst[T](Graph(body.splice))  // careful, intermediate variable would break scalac
      val code          = Pattern.Code(sourceExpr.splice)
      val codeObj       = Code.Obj.newVar[T](Code.Obj.newConst[T](code))
      obj.attr.put(Pattern.attrSource, codeObj)
    }
  }

  // XXX TODO --- DRY
  def streamGraphWithSource[T <: Txn[T]](c: blackbox.Context)(body: c.Expr[Pat[Any]])(tx: c.Expr[T])
                                        (implicit tt: c.WeakTypeTag[T]): c.Expr[Unit] = { // yes, `tt` _is_ used
    import c.universe._

    val source      = mkSource(c)("stream", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    reify {
      implicit val txc  = tx.splice // N.B.: don't annotate the type with `S#Tx`, it will break scalac
      val ext           = c.prefix.splice.asInstanceOf[MacroImplicits.StreamMacroOps[T]]
      val obj           = ext.`this`
      import obj.context
      obj.peer()        = Graph(body.splice).expand[T]  // careful, intermediate variable would break scalac
      val code          = Pattern.Code(sourceExpr.splice)
      val codeObj       = Code.Obj.newVar[T](Code.Obj.newConst[T](code))
      obj.attr.put(Stream.attrSource, codeObj)
    }
  }
}
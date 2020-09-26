/*
 *  MacroImplicits.scala
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

import de.sciss.lucre.Txn
import de.sciss.patterns.Pat
import de.sciss.patterns.lucre.impl.Macros

import scala.language.experimental.macros

/** Enables implicits extensions
  * to assign `Pat[_]`s to a `Pattern.Var` or `Stream` object from a standard IDE,
  * compiling these objects correctly for storage in the workspace,
  * and preserving the corresponding source code.
  */
object MacroImplicits {
  implicit final class PatternMacroOps[T <: Txn[T]](val `this`: Pattern.Var[T]) extends AnyVal {
    def setGraph(body: Pat[Any])(implicit tx: T): Unit =
      macro Macros.patternGraphWithSource[T]
  }

  implicit final class StreamMacroOps[T <: Txn[T]](val `this`: Stream[T]) extends AnyVal {
    def setGraph(body: Pat[Any])(implicit tx: T): Unit =
      macro Macros.streamGraphWithSource[T]
  }
}

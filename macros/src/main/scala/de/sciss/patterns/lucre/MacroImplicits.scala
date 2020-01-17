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

import de.sciss.lucre.stm.Sys
import de.sciss.patterns.Pat
import de.sciss.patterns.lucre.impl.Macros

import scala.language.experimental.macros

/** Enables implicits extensions
  * to assign `Pat[_]`s to a `Pattern.Var` object from a standard IDE,
  * compiling these objects correctly for storage in the workspace,
  * and preserving the corresponding source code.
  */
object MacroImplicits {
  implicit final class PatternMacroOps[S <: Sys[S]](/* private[lucre] */ val `this`: Pattern.Var[S]) extends AnyVal {
    def setGraph(body: Pat[_])(implicit tx: S#Tx): Unit =
      macro Macros.patternGraphWithSource[S]
  }
}

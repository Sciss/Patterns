/*
 *  PatImport.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.lucre

import de.sciss.patterns.graph.{Attribute, Pat}
import de.sciss.patterns
import de.sciss.patterns.graph

import scala.language.implicitConversions

object PatImport extends PatImport
trait PatImport extends patterns.PatImport {
  implicit def audioCueOps[A](p: Pat      [graph.AudioCue]): AudioCueOps  = new AudioCueOps (p)
  implicit def folderOps  [A](p: Attribute[graph.Folder  ]): FolderOps    = new FolderOps   (p)

  implicit def stringToAttr(s: String): graph.Attribute.Factory = new graph.Attribute.Factory(s)
}

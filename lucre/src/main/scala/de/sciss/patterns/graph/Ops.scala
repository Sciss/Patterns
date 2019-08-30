/*
 *  Ops.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns.graph

import de.sciss.patterns.{Pat, graph}
import de.sciss.patterns.lucre.{AudioCueOps, FolderOps}

import scala.language.implicitConversions

object Ops {
  implicit def audioCueOps[A](p: Pat      [graph.AudioCue]): AudioCueOps  = new AudioCueOps (p)
  implicit def folderOps  [A](p: Attribute[graph.Folder  ]): FolderOps    = new FolderOps   (p)

  implicit def stringToAttr(s: String): graph.Attribute.Factory = new graph.Attribute.Factory(s)
}

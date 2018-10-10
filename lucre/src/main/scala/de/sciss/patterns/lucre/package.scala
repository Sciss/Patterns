/*
 *  package.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import scala.language.implicitConversions

package object lucre {
  implicit def audioCueOps[A](p: Pat[graph.AudioCue]): AudioCueOps  = new AudioCueOps (p)
  implicit def folderOps  [A](p: Pat[graph.Folder  ]): FolderOps    = new FolderOps   (p)

  implicit def stringToAttr(s: String): graph.Attribute.Factory = new graph.Attribute.Factory(s)
}

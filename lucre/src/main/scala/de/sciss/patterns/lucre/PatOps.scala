/*
 *  PatOps.scala
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
package lucre

import de.sciss.patterns.graph.Obj

final class AudioCueOps(private val x: Pat[graph.AudioCue]) extends AnyVal {
  def numFrames   : Pat[Long  ] = graph.AudioCue.NumFrames  (x)
  def sampleRate  : Pat[Double] = graph.AudioCue.SampleRate (x)
  def numChannels : Pat[Int   ] = graph.AudioCue.NumChannels(x)
  def duration    : Pat[Double] = ??? // numFrames / sampleRate
}

final class FolderOps(private val x: Pat[graph.Folder]) extends AnyVal {
  def collect[A: Obj.Type]: Pat[Pat[A]] = graph.Folder.Collect[A](x)
}
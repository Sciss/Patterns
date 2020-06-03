/*
 *  AuralProcEvtImpl.scala
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

import de.sciss.lucre.synth.Sys
import de.sciss.patterns.Event
import de.sciss.synth.proc.impl.AuralProcImpl
import de.sciss.synth.proc.{AuralContext, Runner}

// XXX TODO remove in next major version
/** Extends the standard aural proc implementation by injecting scalar values and
  * others (audio-cues) directly from an event value, typically coming from a pattern.
  */
@deprecated("Should use standard AuralProc and runWith", since = "0.19.1")
final class AuralProcEvtImpl[S <: Sys[S]](evt: Event)(implicit context: AuralContext[S])
  extends AuralProcImpl.Impl[S](Runner.emptyAttr /* XXX TODO: evt.map*/ ) {

  throw new IllegalStateException("No longer supported")
}

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

import de.sciss.lucre.synth.{NodeRef, Sys}
import de.sciss.patterns.Event
import de.sciss.patterns.graph.AudioCue
import de.sciss.synth.proc.UGenGraphBuilder.Input.Stream
import de.sciss.synth.proc.UGenGraphBuilder.MissingIn
import de.sciss.synth.proc.graph.Attribute
import de.sciss.synth.proc.impl.AuralProcImpl
import de.sciss.synth.proc.impl.AuralProcImpl.BufferAndGain
import de.sciss.synth.proc.{AuralContext, Runner, TimeRef, UGenGraphBuilder => UGB}

/** Extends the standard aural proc implementation by injecting scalar values and
  * others (audio-cues) directly from an event value, typically coming from a pattern.
  */
final class AuralProcEvtImpl[S <: Sys[S]](evt: Event)(implicit context: AuralContext[S])
  extends AuralProcImpl.Impl[S](Runner.emptyAttr /* XXX TODO: evt.map*/ ) {

  override def requestInput[Res](in: UGB.Input { type Value = Res }, st: UGB.Requester[S])
                                (implicit tx: S#Tx): Res = in match {
    case i: UGB.Input.Scalar if evt.contains(i.name) =>
      val valueOpt    = AuralPatternAttribute.getScalarValue(evt = evt, key = i.name)
      val found: Int  = valueOpt.fold(-1)(_.numChannels)

      import i.{defaultNumChannels => defNum, requiredNumChannels => reqNum}
      if ((found < 0 && i.defaultNumChannels < 0) || (found >= 0 && reqNum >= 0 && found != reqNum)) {
        // throw new IllegalStateException(s"Attribute ${i.name} requires $reqNum channels (found $found)")
        throw MissingIn(i.key)
      }
      val res = if (found >= 0) found else if (reqNum >= 0) reqNum else defNum
      UGB.Input.Scalar.Value(res)

    case i: UGB.Input.Stream if evt.contains(i.name) =>
      val value0 = evt.get(i.name) match {
        case Some(AudioCue(peer)) =>
          val spec = peer.spec
          UGB.Input.Stream.Value(numChannels = spec.numChannels, sampleRate = spec.sampleRate, specs = Nil)
        case _ =>
          simpleInputStreamValue(-1)
      }
      val chansUnknown  = value0.numChannels < 0
      if (chansUnknown && !i.spec.isEmpty) throw MissingIn(i.key)
      val value1        = if (chansUnknown) value0.copy(numChannels = 1) else value0 // simply default to 1
      val newSpecs0     = List.empty[UGB.Input.Stream.Spec]
      val newSpecs      = if (i.spec.isEmpty) newSpecs0 else {
        i.spec :: newSpecs0
      }
      val value2        = if (newSpecs.isEmpty) value1 else value1.copy(specs = newSpecs)
      value2

      // XXX TODO:
//    case i: UGB.Input.Buffer if evt.contains(i.name) =>
//      val procObj   = procCached()
//      val aKey      = i.name
//      val attrValue = procObj.attr.get(aKey).getOrElse(throw MissingIn(i.key))
//      val res0      = requestInputBuffer(aKey, attrValue)
//      // larger files are asynchronously prepared, smaller ones read on the fly
//      val async     = res0.numSamples > UGB.Input.Buffer.AsyncThreshold   // XXX TODO - that threshold should be configurable
//      if (async == res0.async) res0 else res0.copy(async = async)

    case i: UGB.Input.Attribute if evt.contains(i.name) =>
      val opt = evt.map.get(i.name)
      UGB.Input.Attribute.Value(opt)

    case _ =>
      super.requestInput[Res](in, st)
  }

  override protected def buildAttrStreamInput(nr: NodeRef.Full[S], timeRef: TimeRef, key: String,
                                              info: Stream.Spec, idx: Int, bufSize: Int)
                                             (implicit tx: S#Tx): BufferAndGain =
    if (evt.contains(key)) {
      val cue = evt.get(key) match {
        case Some(AudioCue(peer)) => peer
        case _ => throw new IllegalStateException(s"Event key $key not found")
      }
      streamAudioCueToBuffer(cue = cue, nr = nr, timeRef = timeRef, key = key, info = info,
        idx = idx, bufSize = bufSize)
    } else {
      super.buildAttrStreamInput(nr, timeRef, key = key, info = info, idx = idx, bufSize = bufSize)
    }

  override protected def buildAttrInput(nr: NodeRef.Full[S], timeRef: TimeRef, key: String, value: UGB.Value)
                                       (implicit tx: S#Tx): Unit = {
    value match {
      case UGB.Input.Scalar.Value(numChannels) if evt.contains(key) =>  // --------------------- scalar
        val scalarOpt = AuralPatternAttribute.getScalarValue(evt = evt, key = key)
        val scalar    = scalarOpt.getOrElse(throw new IllegalStateException(s"Event key $key not found"))
        val ctlKey    = Attribute.controlName(key)
        nr.addControl(scalar.toControl(key = ctlKey, numChannels = numChannels))

        // XXX TODO:
//      case UGB.Input.Buffer.Value(_ /* numFr */, _ /* numCh */, false) if evt.contains(key) =>   // ----------------------- random access buffer
//      ...

      case _ =>
        super.buildAttrInput(nr, timeRef, key = key, value = value)
    }
  }
}

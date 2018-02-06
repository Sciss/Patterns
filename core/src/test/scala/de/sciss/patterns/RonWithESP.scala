package de.sciss.patterns

import de.sciss.patterns.graph.impl.TimeRef
import de.sciss.osc
import de.sciss.synth.Ops._
import de.sciss.synth.{ugen, _}

import scala.collection.immutable.{SortedMap => ISortedMap}

object RonWithESP {
  def main(args: Array[String]): Unit = {
    Server.run()(run)
  }

  def run(s: Server): Unit = {
    import ugen._

    SynthDef.recv("sine4") {
      val freq    = "freq"  .kr
      val amp     = "amp"   .kr
      val gate    = "gate"  .kr(1.0)
      val ar      = "ar"    .kr
      val dr      = "dr"    .kr(0.6)
      val i       = "i"     .kr
      val index   = "index" .kr(0.5)
      val out     = "out"   .kr
      val freqMod = EnvGen.ar(Env.perc( ar, dr, index))
      val osc     = SinOsc.ar(freq, SinOsc.ar(26 - i * freq / 4) * freqMod) * amp
      val sig     = osc * Linen.kr(gate, ar, 1, dr, 2)
      Out.ar(out + i, sig)
    }

    SynthDef.recv("filter1") {
      val bus = "out".kr
      val in  = In.ar(bus, 24)
      val inX = in.excess(LFNoise1.kr(Seq.fill(24)(Rand(0.0, 0.1) + 0.01)).linexp(-1, 1, 0.00001, 0.002))
      val pch = LFNoise1.kr(Seq.fill(24)(0.1)).linexp(-1, 1, 0.1, 12).max(1)
      val sig = Squiz.ar(inX, pch, (1 to 24: GE).linlin(1, 24, 1.4, 5)) * 0.3 + in
      ReplaceOut.ar(bus, sig)
    }

    object CompanderD {
      def ar(in: GE = 0.0, thresh: GE = 0.5, slopeBelow: GE = 1.0, slopeAbove: GE = 1.0,
             clampTime: GE = 0.01, relaxTime: GE = 0.01, mul: GE = 1.0, add: GE = 0.0): GE = {
        import ugen._
        Compander.ar(DelayN.ar(in, clampTime, clampTime), in, thresh,
          slopeBelow, slopeAbove, clampTime, relaxTime).madd(mul, add)
      }
    }

    implicit class GECat(ge: GE) {
      def ++ (that: GE): GE = Flatten(new GESeq(Vector(ge, that)))
    }
    
    SynthDef.recv("filter2", completion = (_: SynthDef) => player(s)) {
      val bus     = "out".kr
      val in      = In.ar(bus, 24)
      val panned  = SplayAz.ar(2, in)
      val inL     = panned\0
      val inR     = panned\1
      val filter  = HPF.ar(JPverb.ar(inL, inR, 1.0), LFNoise1.kr(0.2).linexp(-1, 1, 100, 1500))
      val sig     = CompanderD.ar(filter * 2, 0.5, 1, 0, 0, 1) ++ Silent.ar(24)
      ReplaceOut.ar(bus, Limiter.ar(sig * 2))
    }
  }

  def player[Tx](s: Server): Unit = {
    val g = Group.tail(s)
    Synth.head(g, "filter1")
    Synth.tail(g, "filter2")

    implicit val ctx: Context.Plain = Context()
    val pat = RonTupleNeu.spawner()
    import ctx.tx
    val it0  = pat.expand

    new Thread {
      override def run(): Unit = {
        val tempo   = 0.25
        type Out    = Event#COut
        var pq      = ISortedMap.empty[TimeRef, Either[Stream[Tx, Out], osc.Message]]
        var now     = 0.0
        var refCnt  = 0

        def mkRef() = {
          val res = new TimeRef(refCnt)
          refCnt += 1
          res
        }

        if (it0.hasNext) ??? // pq += mkRef() -> Left(it0)
        refCnt += 1
        val t0      = System.currentTimeMillis()
        val latency = 100

        val ctlNames = Seq("ar", "dr", "i", "index", "out")

        while (pq.nonEmpty) {
          val (ref, obj) = pq.head
          pq  = pq.tail
          now = ref.time
          val t1 = System.currentTimeMillis()
          val t2 = (t0 + now * 1000).toLong
          val dt = t2 - t1
          if (dt > 0) Thread.sleep(dt)

          obj match {
            case Left(it) =>
              val evt: Out = ??? // it.next()
              val ctl0 = ctlNames.flatMap { key =>
                evt.map.get(key).collect {
                  case d: Double  => key -> d: ControlSet
                  case i: Int     => key -> i: ControlSet
                }
              }
              val freq    = Event.detunedFreq (evt)
              val amp     = Event.amp         (evt)
              val sustain = Event.sustain     (evt) / tempo
              val delta   = Event.delta       (evt) / tempo
              val defName = evt.map.get("instrument").fold("default")(_.toString)

              val ctl     = ctl0 ++ List[ControlSet]("freq" -> freq, "amp" -> amp)

              val syn = Synth(s)
              val m = syn.newMsg(defName, s, args = ctl)
              s ! osc.Bundle.millis(t2 + latency, m)

              ref.time += delta
              ??? // if (it.hasNext) pq += ref -> Left(it)

              val refGate   = mkRef()
              refGate.time  = now + sustain
              pq += refGate -> Right(syn.releaseMsg())

            case Right(m) =>
              s ! osc.Bundle.millis(t2 + latency, m)
          }
        }

        Thread.sleep(1000)
        println("Done.")
        sys.exit()
      }

      start()
    }
  }
}

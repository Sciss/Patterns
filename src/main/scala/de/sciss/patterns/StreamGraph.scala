/*
 *  StreamGraph.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns

import de.sciss.patterns.graph.Constant

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object StreamGraph {
  trait Builder {
    def addStream(stream: Stream[_]): Unit
    def visit[U](ref: AnyRef, init: => U): U
  }
  
  def build(graph: Graph)(implicit ctrl: stream.Control): StreamGraph = {
    val b = new BuilderImpl
    var g0 = graph
    while (g0.nonEmpty) {
      g0 = Graph {
        g0.sources.foreach { source =>
          source.force(b)
        }
      }
    }
    b.build
  }

  // ---- IndexedUGen ----
  final class IndexedUGenBuilder(val stream: Stream[_], var effective: Boolean) {
    var children    : Array[List[IndexedUGenBuilder]]   = ??? // Array.fill(stream.numOutputs)(Nil)
    var inputIndices: List[UGenInIndex]                 = Nil
    var index       : Int                               = -1

    override def toString = s"Idx($stream, $effective) : richInputs = $inputIndices"
  }

  private[patterns] sealed trait UGenInIndex {
    def makeEffective(): Int
  }

  private[patterns] final class ConstantIndex(val peer: Constant[_]) extends UGenInIndex {
    def makeEffective() = 0

    override def toString: String = peer.toString
  }

  private[patterns] final class UGenProxyIndex(val iu: IndexedUGenBuilder, val outIdx: Int) extends UGenInIndex {
    def makeEffective(): Int = {
      if (!iu.effective) {
        iu.effective = true
        var numEff   = 1
        iu.inputIndices.foreach(numEff += _.makeEffective())
        numEff
      } else 0
    }

    override def toString = s"$iu[$outIdx]"
  }

  private final class BuilderImpl extends BuilderLike

  // - converts to StreamIn objects that automatically insert stream broadcasters
  //   and dummy sinks
  def buildStream(streams: Vec[IndexedUGenBuilder])(implicit ctrl: stream.Control): Any /* RunnableGraph[NotUsed] */ = {
    // empty graphs are not supported by Akka
    ???

//    if (ugens.isEmpty) throw new IllegalStateException("Graph is empty")
//    val _graph = GraphDSL.create() { implicit dsl =>
//      implicit val sb = stream.Builder()
//
//      var ugenOutMap = Map.empty[IndexedUGenBuilder, Array[StreamIn]]
//
//      ugens.foreach { iu =>
//        val args: Vec[StreamIn] = iu.inputIndices.map {
//          case c: ConstantIndex   => c.peer
//          case u: UGenProxyIndex  => ugenOutMap(u.iu)(u.outIdx)
//        } (breakOut)
//
//        @inline def add(value: Array[StreamIn]): Unit = {
//          // println(s"map += $iu -> ${value.mkString("[", ", ", "]")}")
//          ugenOutMap += iu -> value
//        }
//
//        iu.ugen match {
//          case ugen: UGen.SingleOut =>
//            val out         = ugen.source.makeStream(args)
//            val numChildren = iu.children(0).size
//            val value       = Array(out.toIn(numChildren))
//            add(value)
//
//          case ugen: UGen.MultiOut  =>
//            val outs  = ugen.source.makeStream(args)
//            val value = outs.zipWithIndex.map { case (out, outIdx) =>
//              val numChildren = iu.children(outIdx).size
//              out.toIn(numChildren)
//            } (breakOut) : Array[StreamIn]
//            add(value)
//
//          case ugen: UGen.ZeroOut   =>
//            ugen.source.makeStream(args)
//        }
//      }
//      ClosedShape
//    }
//    RunnableGraph.fromGraph(_graph)
  }


  // - builds parent-child graph of UGens
  // - deletes no-op sub-trees
  def indexStreams(streams: Vec[Stream[_]]): Vec[IndexedUGenBuilder] = {
    var numIneffective  = streams.size
    val indexedUGens    = streams.map { ugen =>
      val eff = true // ugen.hasSideEffect
      if (eff) numIneffective -= 1
      new IndexedUGenBuilder(ugen, eff)
    }

    val ugenMap: Map[AnyRef, IndexedUGenBuilder] = indexedUGens.map(iu => (iu.stream, iu))(breakOut)
    indexedUGens.foreach { iu =>
      iu.inputIndices = iu.stream.inputs.map {
        case c: Constant[_] =>
          new ConstantIndex(c)

//        case up: StreamProxy =>
//          val iui = ugenMap(up.stream)
//          iui.children(up.outputIndex) ::= iu
//          new UGenProxyIndex(iui, up.outputIndex)
      } (breakOut)
      if (iu.effective) iu.inputIndices.foreach(numIneffective -= _.makeEffective())
    }
    val filtered: Vec[IndexedUGenBuilder] =
      if (numIneffective == 0)
        indexedUGens
      else
        indexedUGens.collect {
          case iu if iu.effective =>
            for (outputIndex <- iu.children.indices) {
              iu.children(outputIndex) = iu.children(outputIndex).filter(_.effective)
            }
            iu
        }

    filtered
  }

  private[patterns] trait BuilderLike extends Builder {
    // ---- abstract ----

    //    implicit protected def ctrl: stream.Control

    // ---- impl ----

    private[this] var _streams = Vector.empty[Stream[_]]
    // private[this] val ugenSet   = mutable.Set.empty[UGen]
    private[this] var sourceMap = Map.empty[AnyRef, Any]

    protected final def streams: Vec[Stream[_]] = _streams

    def build(implicit ctrl: stream.Control): StreamGraph = {
      val iUGens  = indexStreams(_streams)
      val rg      = buildStream(iUGens)
      StreamGraph(rg)
    }

    private def printSmart(x: Any): String = x match {
      case u: Stream[_]     => u.name
      // case p: ChannelProxy  => s"${printSmart(p.elem)}.\\(${p.index})"
      case _                => x.toString
    }

    @inline
    private def printRef(ref: AnyRef): String = {
      val hash = ref.hashCode.toHexString
      //      ref match {
      //        case p: Product => s"${p.productPrefix}@$hash"
      //        case _ => hash
      //      }
      hash
    }

    def visit[U](ref: AnyRef, init: => U): U = {
      logGraph(s"visit  ${printRef(ref)}")
      sourceMap.getOrElse(ref, {
        logGraph(s"expand ${printRef(ref)}...")
        val exp = init
        logGraph(s"...${printRef(ref)} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
        sourceMap += ref -> exp
        exp
      }).asInstanceOf[U] // not so pretty...
    }

    def addStream(stream: Stream[_]): Unit = {
      // Where is this check in ScalaCollider? Have we removed it (why)?
      // N.B.: We do not use UGen equality any longer in FScape because
      // we might need to feed the same structure into different sinks
      // that read at different speeds, so we risk to block the graph
      // (Imagine a `DC(0.0)` going into two entirely different places!)

      // if (ugenSet.add(ugen)) {
      _streams :+= stream
      logGraph(s"addUGen ${stream.name} @ ${stream.hashCode.toHexString}")
      // } else {
      //  logGraph(s"addUGen ${ugen.name} @ ${ugen.hashCode.toHexString} - duplicate")
      // }
    }
  }
}
final case class StreamGraph(runnable: Any)
/*
 *  Context.scala
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

import scala.util.Random

trait Context {
//  def visit[U](ref: AnyRef, init: => U): U

  def addStream[A](ref: AnyRef, stream: Stream[A]): Stream[A]

  def getStreams(ref: AnyRef): List[Stream[_]]

  def mkRandom(): Random
}

object Context {
  def apply(): Context = new Impl

  private final class Impl extends ContextLike {
    private[this] lazy val seedRnd = new Random()

    def mkRandom(): Random = new Random(seedRnd.nextLong())
  }
}

private[patterns] abstract class ContextLike extends Context {
  private[this] var streamMap = Map.empty[AnyRef, List[Stream[_]]]

//  private[this] var sourceMap = Map.empty[AnyRef, Any]
//
//  private def printSmart(x: Any): String = x match {
////    case s: Stream[_]     => s.name
//    case _                => x.toString
//  }
//
//  @inline
//  private def printRef(ref: AnyRef): String = {
//    val hash = ref.hashCode.toHexString
//    hash
//  }
//
//  def visit[U](ref: AnyRef, init: => U): U = {
//    logGraph(s"visit  ${printRef(ref)}")
//    sourceMap.getOrElse(ref, {
//      logGraph(s"expand ${printRef(ref)}...")
//      val exp = init
//      logGraph(s"...${printRef(ref)} -> ${exp.hashCode.toHexString} ${printSmart(exp)}")
//      sourceMap += ref -> exp
//      exp
//    }).asInstanceOf[U] // not so pretty...
//  }

  def addStream[A](ref: AnyRef, stream: Stream[A]): Stream[A] = {
    streamMap += ref -> (stream :: streamMap.getOrElse(ref, Nil))
    stream
  }

  def getStreams(ref: AnyRef): List[Stream[_]] = streamMap.getOrElse(ref, Nil)
}
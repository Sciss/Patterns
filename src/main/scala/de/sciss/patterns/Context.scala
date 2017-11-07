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

trait Context {
  def visit[U](ref: AnyRef, init: => U): U
}

object Context {
  def apply(): Context = new Impl

  private final class Impl extends ContextLike
}

private[patterns] trait ContextLike extends Context {

  private[this] var sourceMap = Map.empty[AnyRef, Any]

  private def printSmart(x: Any): String = x match {
//    case s: Stream[_]     => s.name
    case _                => x.toString
  }

  @inline
  private def printRef(ref: AnyRef): String = {
    val hash = ref.hashCode.toHexString
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
}
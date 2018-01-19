package de.sciss.patterns

abstract class StreamNew[+A] {
  def reset(): Unit

  def moveNext(): Boolean

  def current: A
}
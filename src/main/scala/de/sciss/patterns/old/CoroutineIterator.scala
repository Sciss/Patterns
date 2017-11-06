package de.sciss.patterns.old

import org.coroutines.Coroutine

final class CoroutineIterator[A](peer: Coroutine.Instance[A, _]) extends Iterator[A] {

  private[this] var hasPulled = false

  private[this] var _hasNext: Boolean = _

  private def pull(): Unit =
    if (!hasPulled) {
      _hasNext = peer.pull
      hasPulled = true
    }

  def hasNext: Boolean = {
    pull()
    _hasNext
  }

  def next(): A = {
    pull()
    val res   = peer.value
    hasPulled = false
    res
  }
}
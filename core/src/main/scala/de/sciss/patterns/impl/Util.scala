//package de.sciss.patterns
//package impl
//
//import scala.collection.immutable.{IndexedSeq => Vec}
//
//object Util {
//  implicit def vecNumeric[A](implicit peer: Numeric[A]): Numeric[Vec[A]] = new VecNumeric[A](peer)
//
//  private final class VecNumeric[A](peer: Numeric[A]) extends Numeric[Vec[A]] {
//    private def map(x: Vec[A], y: Vec[A])(op: (A, A) => A): Vec[A] = {
//      val sx = x.size
//      val sy = y.size
//      if (sx == 0 || sy == 0) return Vector.empty
//      val sz = math.max(sx, sy)
//      Vector.tabulate(sz) { i =>
//        val xi = x(i % sx)
//        val yi = y(i % sy)
//        op(xi, yi)
//      }
//    }
//
//    def plus  (x: Vec[A], y: Vec[A]): Vec[A] = map(x, y)(peer.plus )
//    def minus (x: Vec[A], y: Vec[A]): Vec[A] = map(x, y)(peer.minus)
//    def times (x: Vec[A], y: Vec[A]): Vec[A] = map(x, y)(peer.times)
//
//    def negate(x: Vec[A]): Vec[A] = x.map(peer.negate)
//
//    def fromInt(x: Int): Vec[A] = Vector(peer.fromInt(x))
//
//    def toInt   (x: Vec[A]): Int    = throw new UnsupportedOperationException("toInt")
//    def toLong  (x: Vec[A]): Long   = throw new UnsupportedOperationException("toLong")
//    def toFloat (x: Vec[A]): Float  = throw new UnsupportedOperationException("toFloat")
//    def toDouble(x: Vec[A]): Double = throw new UnsupportedOperationException("toDouble")
//
//    def compare(x: Vec[A], y: Vec[A]): Int = throw new UnsupportedOperationException("compare")
//  }
//}

package de.sciss.patterns

package impl

import de.sciss.lucre.stm.Random
import de.sciss.patterns.Types._

trait SeqLike[A] {
  final type Boolean  = Seq[scala.Boolean ]
  final type Int      = Seq[scala.Int     ]
  final type Double   = Seq[scala.Double  ]
  final type In       = Seq[A]

  final protected def unOp[B](a: In)(op: A => B): Seq[B] = a.map(op)

  final protected def binOp[B](a: In, b: In)(op: (A, A) => B): Seq[B] = {
    val as = a.size
    val bs = b.size
    val sz = math.max(as, bs)
    Seq.tabulate(sz) { i =>
      op(a(i % as), b(i % bs))
    }
  }

  final protected def ternOp(a: In, b: In, c: In)(op: (A, A, A) => A): Seq[A] = {
    val as = a.size
    val bs = b.size
    val cs = c.size
    val sz = math.max(math.max(as, bs), cs)
    Seq.tabulate(sz) { i =>
      op(a(i % as), b(i % bs), c(i % cs))
    }
  }
}

trait SeqLikeEq[A] extends SeqLike[A] with Eq[Seq[A]] {
  protected val peer: ScalarEq[A]

  def eq (a: In, b: In): Boolean = binOp(a, b)(peer.eq )
  def neq(a: In, b: In): Boolean = binOp(a, b)(peer.neq)
}

trait SeqLikeOrd[A] extends SeqLikeEq[A] with Ord[Seq[A]] {
  protected val peer: ScalarOrd[A]

  def lt  (a: In, b: In): Boolean = binOp(a, b)(peer.lt  )
  def leq (a: In, b: In): Boolean = binOp(a, b)(peer.leq )
  def gt  (a: In, b: In): Boolean = binOp(a, b)(peer.gt  )
  def geq (a: In, b: In): Boolean = binOp(a, b)(peer.geq )
}

trait SeqLikeNum[A] extends SeqLikeOrd[A] with Num[Seq[A]] {
  override protected val peer: ScalarNum[A]

  final def +(a: In, b: In): In = binOp(a, b)(peer.+      )
  final def -(a: In, b: In): In = binOp(a, b)(peer.-     )
  final def *(a: In, b: In): In = binOp(a, b)(peer.*     )
  final def %         (a: In, b: In): In = binOp(a, b)(peer.%         )
  final def mod       (a: In, b: In): In = binOp(a, b)(peer.mod       )
  final def min       (a: In, b: In): In = binOp(a, b)(peer.min       )
  final def max       (a: In, b: In): In = binOp(a, b)(peer.max       )
  final def roundTo   (a: In, b: In): In = binOp(a, b)(peer.roundTo   )
  final def roundUpTo (a: In, b: In): In = binOp(a, b)(peer.roundUpTo )
  final def trunc     (a: In, b: In): In = binOp(a, b)(peer.trunc     )
  final def difsqr    (a: In, b: In): In = binOp(a, b)(peer.difsqr    )
  final def sumsqr    (a: In, b: In): In = binOp(a, b)(peer.sumsqr    )
  final def sqrsum    (a: In, b: In): In = binOp(a, b)(peer.sqrsum    )
  final def sqrdif    (a: In, b: In): In = binOp(a, b)(peer.sqrdif    )
  final def absdif    (a: In, b: In): In = binOp(a, b)(peer.absdif    )

  final def clip2     (a: In, b: In): In = binOp(a, b)(peer.clip2     )
  final def excess    (a: In, b: In): In = binOp(a, b)(peer.excess    )
  final def fold2     (a: In, b: In): In = binOp(a, b)(peer.fold2     )
  final def wrap2     (a: In, b: In): In = binOp(a, b)(peer.wrap2     )

  final def negate    (a: In): In   = unOp(a)(peer.negate )
  final def abs       (a: In): In   = unOp(a)(peer.abs    )
  final def signum    (a: In): In   = unOp(a)(peer.signum )

  final def squared   (a: In): In   = unOp(a)(peer.squared)
  final def cubed     (a: In): In   = unOp(a)(peer.cubed  )

  final def zero : In = peer.zero :: Nil
  final def one  : In = peer.one  :: Nil

  final def rand [Tx](a: In       )(implicit r: Random[Tx], tx: Tx): In = unOp (a   )(peer.rand [Tx])
  final def rand2[Tx](a: In       )(implicit r: Random[Tx], tx: Tx): In = unOp (a   )(peer.rand2[Tx])
  final def rrand[Tx](a: In, b: In)(implicit r: Random[Tx], tx: Tx): In = binOp(a, b)(peer.rrand[Tx])

  final def fold(a: In, lo: In, hi: In): In = ternOp(a, lo, hi)(peer.fold)
}

trait SeqLikeNumFrac[A] extends SeqLikeNum[A] with NumFrac[Seq[A]] {
  override protected val peer: ScalarNumFrac[A]

  final def floor     (a: In): In = unOp(a)(peer.floor      )
  final def ceil      (a: In): In = unOp(a)(peer.ceil       )
  final def frac      (a: In): In = unOp(a)(peer.frac       )
  final def reciprocal(a: In): In = unOp(a)(peer.reciprocal )

  final def /         (a: In, b: In): In = binOp(a, b)(peer./)
}

trait SeqLikeNumDouble[A] extends SeqLikeNumFrac[A] with NumDouble[Seq[A]] {
  override protected val peer: ScalarNumDouble[A]

  final def sqrt      (a: In): In = unOp(a)(peer.sqrt     )
  final def exp       (a: In): In = unOp(a)(peer.exp      )

  final def midicps   (a: In): In = unOp(a)(peer.midicps  )
  final def cpsmidi   (a: In): In = unOp(a)(peer.cpsmidi  )
  final def midiratio (a: In): In = unOp(a)(peer.midiratio)
  final def ratiomidi (a: In): In = unOp(a)(peer.ratiomidi)
  final def dbamp     (a: In): In = unOp(a)(peer.dbamp    )
  final def ampdb     (a: In): In = unOp(a)(peer.ampdb    )
  final def octcps    (a: In): In = unOp(a)(peer.octcps   )
  final def cpsoct    (a: In): In = unOp(a)(peer.cpsoct   )
  final def log       (a: In): In = unOp(a)(peer.log      )
  final def log2      (a: In): In = unOp(a)(peer.log2     )
  final def log10     (a: In): In = unOp(a)(peer.log10    )
  final def sin       (a: In): In = unOp(a)(peer.sin      )
  final def cos       (a: In): In = unOp(a)(peer.cos      )
  final def tan       (a: In): In = unOp(a)(peer.tan      )
  final def asin      (a: In): In = unOp(a)(peer.asin     )
  final def acos      (a: In): In = unOp(a)(peer.acos     )
  final def atan      (a: In): In = unOp(a)(peer.atan     )
  final def sinh      (a: In): In = unOp(a)(peer.sinh     )
  final def cosh      (a: In): In = unOp(a)(peer.cosh     )
  final def tanh      (a: In): In = unOp(a)(peer.tanh     )

  final def atan2     (a: In, b: In): In = binOp(a, b)(peer.atan2 )
  final def hypot     (a: In, b: In): In = binOp(a, b)(peer.hypot )
  final def hypotx    (a: In, b: In): In = binOp(a, b)(peer.hypotx)
  final def pow       (a: In, b: In): In = binOp(a, b)(peer.pow   )

  def coin[Tx](a: In)(implicit r: Random[Tx], tx: Tx): Boolean = unOp(a)(peer.coin[Tx])
}

trait SeqLikeToNum[A] extends SeqLike[A] with ToNum[Seq[A]] {
  protected val peer: ScalarToNum[A]

  final def toInt   (a: In): Int     = unOp(a)(peer.toInt   )
  final def toDouble(a: In): Double  = unOp(a)(peer.toDouble)

  final def int    : NumInt   [Int    ] = IntSeqTop
  final def double : NumDouble[Double ] = DoubleSeqTop
}

trait ScalarToNumImpl[A] extends ToNum[A] with Scalar[A] {
  final def int   : NumInt   [Int]    = IntTop
  final def double: NumDouble[Double] = DoubleTop
}

trait ScalarEqImpl[A] extends Eq[A] with Scalar[A] {
  final def eq  (a: A, b: A): Boolean = a == b
  final def neq (a: A, b: A): Boolean = a != b
}

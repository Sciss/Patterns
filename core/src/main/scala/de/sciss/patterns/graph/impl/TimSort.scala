// straight translation from Java

/*
 * Copyright (C) 2008 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.sciss.patterns
package graph
package impl

import java.util.Comparator

import scala.annotation.tailrec
import scala.reflect.ClassTag

object TimSort {
  private final val MIN_MERGE = 32

  /**
    * When we get into galloping mode, we stay there until both runs win less
    * often than MIN_GALLOP consecutive times.
    */
  private final val MIN_GALLOP = 7

  /**
    * Maximum initial size of tmp array, which is used for merging.  The array
    * can grow to accommodate demand.
    *
    * Unlike Tim's original C version, we do not allocate this much storage
    * when sorting smaller arrays.  This change was required for performance.
    */
  private final val INITIAL_TMP_STORAGE_LENGTH = 256

  /*
   * The next two methods (which are package private and static) constitute
   * the entire API of this class.  Each of these methods obeys the contract
   * of the public method with the same signature in java.util.Arrays.
   */
  def sort[T](a: Array[T])(implicit ord: Ordering[T], ct: ClassTag[T]): Unit = {
    sort(a, 0, a.length, ord)
  }

  private def sort[T: ClassTag](a: Array[T], lo0: Int, hi: Int, c: Comparator[T]): Unit = {
    require (lo0 >= 0 && hi <= a.length)
    var nRemaining  = hi - lo0
    if (nRemaining < 2) return  // Arrays of size 0 and 1 are always sorted

    // If array is small, do a "mini-TimSort" with no merges
    if (nRemaining < MIN_MERGE) {
      val initRunLen = countRunAndMakeAscending(a, lo0, hi, c)
      binarySort(a, lo0, hi, lo0 + initRunLen, c)
      return
    }

    /**
      * March over the array once, left to right, finding natural runs,
      * extending short natural runs to minRun elements, and merging runs
      * to maintain stack invariant.
      */
    val ts = new TimSort[T](a, c)
    val minRun = minRunLength(nRemaining)
    var lo = lo0
    do {
      // Identify next run
      var runLen = countRunAndMakeAscending(a, lo, hi, c)
      // If run is short, extend to min(minRun, nRemaining)
      if (runLen < minRun) {
        val force = if (nRemaining <= minRun) nRemaining else minRun
        binarySort(a, lo, lo + force, lo + runLen, c)
        runLen = force
      }
      // Push run onto pending-run stack, and maybe merge
      ts.pushRun(lo, runLen)
      ts.mergeCollapse()
      // Advance to find next run
      lo += runLen
      nRemaining -= runLen
    } while (nRemaining != 0)
    // Merge all remaining runs to complete sort
    ts.mergeForceCollapse()
  }

  /**
    * Sorts the specified portion of the specified array using a binary
    * insertion sort.  This is the best method for sorting small numbers
    * of elements.  It requires O(n log n) compares, but O(n pow 2) data
    * movement (worst case).
    *
    * If the initial part of the specified range is already sorted,
    * this method can take advantage of it: the method assumes that the
    * elements from index `lo`, inclusive, to `start`,
    * exclusive are already sorted.
    *
    * @param a the array in which a range is to be sorted
    * @param lo the index of the first element in the range to be sorted
    * @param hi the index after the last element in the range to be sorted
    * @param start0 the index of the first element in the range that is
    *        not already known to be sorted (@code lo <= start <= hi}
    * @param c comparator to used for the sort
    */
  private def binarySort[T](a: Array[T], lo: Int, hi: Int, start0: Int, c: Comparator[T]): Unit = {
    var start = start0
    if (start == lo) start += 1
    while (start < hi) {
      val pivot = a(start)
      // Set left (and right) to the index where a[start] (pivot) belongs
      var left = lo
      var right = start
      /*
       * Invariants:
       *   pivot >= all in [lo, left).
       *   pivot <  all in [right, start).
       */
      while (left < right) {
        val mid = (left + right) >>> 1
        if (c.compare(pivot, a(mid)) < 0)
          right = mid
        else
          left = mid + 1
      }
      /*
       * The invariants still hold: pivot >= all in [lo, left) and
       * pivot < all in [left, start), so pivot belongs at left.  Note
       * that if there are elements equal to pivot, left points to the
       * first slot after them -- that's why this sort is stable.
       * Slide elements over to make room for pivot.
       */
      val n = start - left   // The number of elements to move
      // Switch is just an optimization for arraycopy in default case
      if (n == 2) {
        a(left + 2) = a(left + 1)
        a(left + 1) = a(left)
      } else if (n == 1) {
        a(left + 1) = a(left)
      } else {
        System.arraycopy(a, left, a, left + 1, n)
      }

      a(left) = pivot
        start += 1
    }
  }

  /**
    * Returns the length of the run beginning at the specified position in
    * the specified array and reverses the run if it is descending (ensuring
    * that the run will always be ascending when the method returns).
    *
    * A run is the longest ascending sequence with:
    *
    *    a[lo] <= a[lo + 1] <= a[lo + 2] <= ...
    *
    * or the longest descending sequence with:
    *
    *    a[lo] >  a[lo + 1] >  a[lo + 2] >  ...
    *
    * For its intended use in a stable mergesort, the strictness of the
    * definition of "descending" is needed so that the call can safely
    * reverse a descending sequence without violating stability.
    *
    * @param a the array in which a run is to be counted and possibly reversed
    * @param lo index of the first element in the run
    * @param hi index after the last element that may be contained in the run.
            It is required that @code{lo < hi}.
    * @param c the comparator to used for the sort
    * @return  the length of the run beginning at the specified position in
    *          the specified array
    */
  private def countRunAndMakeAscending[T](a: Array[T], lo: Int, hi: Int, c: Comparator[T]): Int = {
    var runHi = lo + 1
    if (runHi == hi)
      return 1

    // Find end of run, and reverse range if descending
    val ar = a(runHi)
    runHi += 1
    if (c.compare(ar, a(lo)) < 0) { // Descending
      while(runHi < hi && c.compare(a(runHi), a(runHi - 1)) < 0) runHi += 1
      reverseRange(a, lo, runHi)
    } else {                              // Ascending
      while (runHi < hi && c.compare(a(runHi), a(runHi - 1)) >= 0) runHi += 1
    }
    runHi - lo
  }

  /**
    * Reverse the specified range of the specified array.
    *
    * @param a the array in which a range is to be reversed
    * @param lo0 the index of the first element in the range to be reversed
    * @param hi0 the index after the last element in the range to be reversed
    */
  private def reverseRange[T](a: Array[T], lo0: Int, hi0: Int): Unit = {
    var lo = lo0
    var hi = hi0 - 1
    while (lo < hi) {
      val t = a(lo)
      a(lo) = a(hi)
      lo += 1
      a(hi) = t
      hi -= 1
    }
  }

  /**
    * Returns the minimum acceptable run length for an array of the specified
    * length. Natural runs shorter than this will be extended with
    * `binarySort`.
    *
    * Roughly speaking, the computation is:
    *
    *  If n < MIN_MERGE, return n (it's too small to bother with fancy stuff).
    *  Else if n is an exact power of 2, return MIN_MERGE/2.
    *  Else return an int k, MIN_MERGE/2 <= k <= MIN_MERGE, such that n/k
    *   is close to, but strictly less than, an exact power of 2.
    *
    * For the rationale, see listsort.txt.
    *
    * @param n0 the length of the array to be sorted
    * @return the length of the minimum run to be merged
    */
  private def minRunLength(n0: Int): Int = {
    var r = 0      // Becomes 1 if any 1 bits are shifted off
    var n = n0
    while (n >= MIN_MERGE) {
      r |= (n & 1)
      n >>= 1
    }
    n + r
  }

  /**
    * Locates the position at which to insert the specified key into the
    * specified sorted range; if the range contains an element equal to key,
    * returns the index of the leftmost equal element.
    *
    * @param key the key whose insertion point to search for
    * @param a the array in which to search
    * @param base the index of the first element in the range
    * @param len the length of the range; must be > 0
    * @param hint the index at which to begin the search, 0 <= hint < n.
    *     The closer hint is to the result, the faster this method will run.
    * @param c the comparator used to order the range, and to search
    * @return the int k,  0 <= k <= n such that a[b + k - 1] < key <= a[b + k],
    *    pretending that a[b - 1] is minus infinity and a[b + n] is infinity.
    *    In other words, key belongs at index b + k; or in other words,
    *    the first k elements of a should precede key, and the last n - k
    *    should follow it.
    */
  private def gallopLeft[T](key: T, a: Array[T], base: Int, len: Int, hint: Int, c: Comparator[T]): Int = {
    var lastOfs = 0
    var ofs     = 1
    if (c.compare(key, a(base + hint)) > 0) {
      // Gallop right until a[base+hint+lastOfs] < key <= a[base+hint+ofs]
      val maxOfs = len - hint
      while (ofs < maxOfs && c.compare(key, a(base + hint + ofs)) > 0) {
        lastOfs = ofs
        ofs = (ofs * 2) + 1
        if (ofs <= 0) // int overflow
          ofs = maxOfs
      }
      if (ofs > maxOfs) ofs = maxOfs

      // Make offsets relative to base
      lastOfs += hint
      ofs += hint
    } else { // key <= a[base + hint]
      // Gallop left until a[base+hint-ofs] < key <= a[base+hint-lastOfs]
      val maxOfs = hint + 1
      while (ofs < maxOfs && c.compare(key, a(base + hint - ofs)) <= 0) {
        lastOfs = ofs
        ofs = (ofs * 2) + 1
        if (ofs <= 0) // int overflow
          ofs = maxOfs
      }
      if (ofs > maxOfs) ofs = maxOfs

      // Make offsets relative to base
      val tmp = lastOfs
      lastOfs = hint - ofs
      ofs = hint - tmp
    }

    /*
     * Now a[base+lastOfs] < key <= a[base+ofs], so key belongs somewhere
     * to the right of lastOfs but no farther right than ofs.  Do a binary
     * search, with invariant a[base + lastOfs - 1] < key <= a[base + ofs].
     */
    lastOfs += 1
    while (lastOfs < ofs) {
      val m = lastOfs + ((ofs - lastOfs) >>> 1)
      if (c.compare(key, a(base + m)) > 0)
        lastOfs = m + 1 // a[base + m] < key
      else
        ofs = m         // key <= a[base + m]
    }
    ofs
  }

  /**
    * Like gallopLeft, except that if the range contains an element equal to
    * key, gallopRight returns the index after the rightmost equal element.
    *
    * @param key  the key whose insertion point to search for
    * @param a    the array in which to search
    * @param base the index of the first element in the range
    * @param len  the length of the range; must be > 0
    * @param hint the index at which to begin the search, 0 &lt;= hint &lt; n.
    *             The closer hint is to the result, the faster this method will run.
    * @param c    the comparator used to order the range, and to search
    * @return the int k,  0 &lt;= k &lt;= n such that a[b + k - 1] &lt;= key &lt; a[b + k]
    */
  private def gallopRight[T](key: T, a: Array[T], base: Int, len: Int, hint: Int, c: Comparator[T]): Int = {
    var ofs     = 1
    var lastOfs = 0
    if (c.compare(key, a(base + hint)) < 0) {
      // Gallop left until a[b+hint - ofs] <= key < a[b+hint - lastOfs]
      val maxOfs = hint + 1
      while (ofs < maxOfs && c.compare(key, a(base + hint - ofs)) < 0) {
        lastOfs = ofs
        ofs = (ofs * 2) + 1
        if (ofs <= 0) // int overflow
          ofs = maxOfs
      }
      if (ofs > maxOfs) ofs = maxOfs

      // Make offsets relative to b
      val tmp = lastOfs
      lastOfs = hint - ofs
      ofs = hint - tmp
    } else { // a[b + hint] <= key
      // Gallop right until a[b+hint + lastOfs] <= key < a[b+hint + ofs]
      val maxOfs = len - hint
      while (ofs < maxOfs && c.compare(key, a(base + hint + ofs)) >= 0) {
        lastOfs = ofs
        ofs = (ofs * 2) + 1
        if (ofs <= 0) // int overflow
          ofs = maxOfs
      }
      if (ofs > maxOfs) ofs = maxOfs

      // Make offsets relative to b
      lastOfs += hint
      ofs += hint
    }

    /*
         * Now a[b + lastOfs] <= key < a[b + ofs], so key belongs somewhere to
         * the right of lastOfs but no farther right than ofs.  Do a binary
         * search, with invariant a[b + lastOfs - 1] <= key < a[b + ofs].
         */
    lastOfs += 1
    while (lastOfs < ofs) {
      val m = lastOfs + ((ofs - lastOfs) >>> 1)
      if (c.compare(key, a(base + m)) < 0)
        ofs = m; // key < a[b + m]
      else
        lastOfs = m + 1; // a[b + m] <= key
    }
    ofs
  }
}
/**
  * A stable, adaptive, iterative mergesort that requires far fewer than
  * n lg(n) comparisons when running on partially sorted arrays, while
  * offering performance comparable to a traditional mergesort when run
  * on random arrays.  Like all proper mergesorts, this sort is stable and
  * runs O(n log n) time (worst case).  In the worst case, this sort requires
  * temporary storage space for n/2 object references; in the best case,
  * it requires only a small constant amount of space.
  *
  * This implementation was adapted from Tim Peters's list sort for
  * Python, which is described in detail here:
  *
  *   http://svn.python.org/projects/python/trunk/Objects/listsort.txt
  *
  * Tim's C code may be found here:
  *
  *   http://svn.python.org/projects/python/trunk/Objects/listobject.c
  *
  * The underlying techniques are described in this paper (and may have
  * even earlier origins):
  *
  *  "Optimistic Sorting and Information Theoretic Complexity"
  *  Peter McIlroy
  *  SODA (Fourth Annual ACM-SIAM Symposium on Discrete Algorithms),
  *  pp 467-474, Austin, Texas, 25-27 January 1993.
  *
  * While the API to this class consists solely of static methods, it is
  * (privately) instantiable; a TimSort instance holds the state of an ongoing
  * sort, assuming the input array is large enough to warrant the full-blown
  * TimSort. Small arrays are sorted in place, using a binary insertion sort.
  */
class TimSort[T: ClassTag](a: Array[T], c: Comparator[T]) {

  import TimSort._

  /**
   * This is the minimum sized sequence that will be merged.  Shorter
   * sequences will be lengthened by calling binarySort.  If the entire
   * array is less than this length, no merges will be performed.
   *
   * This constant should be a power of two.  It was 64 in Tim Peter's C
   * implementation, but 32 was empirically determined to work better in
   * this implementation.  In the unlikely event that you set this constant
   * to be a number that's not a power of two, you'll need to change the
   * `minRunLength` computation.
   *
   * If you decrease this constant, you must change the stackLen
   * computation in the TimSort constructor, or you risk an
   * ArrayOutOfBounds exception.  See listsort.txt for a discussion
   * of the minimum stack length required as a function of the length
   * of the array being sorted and the minimum merge sequence length.
   */

  /**
   * This controls when we get *into* galloping mode.  It is initialized
   * to MIN_GALLOP.  The mergeLo and mergeHi methods nudge it higher for
   * random data, and lower for highly structured data.
   */
  private[this] var minGallop = MIN_GALLOP
  /**
   * Temp storage for merges.
   */
  private[this] var tmp: Array[T] = { // Actual runtime type will be Object[], regardless of T
    val len = a.length
    new Array[T](if (len < 2 * INITIAL_TMP_STORAGE_LENGTH) len >>> 1 else INITIAL_TMP_STORAGE_LENGTH)
  }

  /**
   * A stack of pending runs yet to be merged.  Run i starts at
   * address base[i] and extends for len[i] elements.  It's always
   * true (so long as the indices are in bounds) that:
   *
   *     runBase[i] + runLen[i] == runBase[i + 1]
   *
   * so we could cut the storage for this, but it's a minor amount,
   * and keeping all the info explicit simplifies the code.
   */
  private[this] var stackSize = 0 // Number of pending runs on stack

  /*
   * Allocate runs-to-be-merged stack (which cannot be expanded).  The
   * stack length requirements are described in listsort.txt.  The C
   * version always uses the same stack length (85), but this was
   * measured to be too expensive when sorting "mid-sized" arrays (e.g.,
   * 100 elements) in Java.  Therefore, we use smaller (but sufficiently
   * large) stack lengths for smaller arrays.  The "magic numbers" in the
   * computation below must be changed if MIN_MERGE is decreased.  See
   * the MIN_MERGE declaration above for more information.
   */
  private[this] val stackLen0 = {
    val len = a.length
    if      (len <    120)  5
    else if (len <   1542) 10
    else if (len < 119151) 19
    else                   40
  }

  private[this] final val runBase = new Array[Int](stackLen0)
  private[this] final val runLen  = new Array[Int](stackLen0)

  /**
   * Pushes the specified run onto the pending-run stack.
   *
   * @param _runBase index of the first element in the run
   * @param _runLen  the number of elements in the run
   */
  private def pushRun(_runBase: Int, _runLen: Int): Unit = {
    runBase(stackSize) = _runBase
    runLen (stackSize) = _runLen
    stackSize += 1
  }

  /**
   * Examines the stack of runs waiting to be merged and merges adjacent runs
   * until the stack invariants are reestablished:
   *
   *     1. runLen[i - 3] > runLen[i - 2] + runLen[i - 1]
   *     2. runLen[i - 2] > runLen[i - 1]
   *
   * This method is called each time a new run is pushed onto the stack,
   * so the invariants are guaranteed to hold for i < stackSize upon
   * entry to the method.
   */
  private def mergeCollapse(): Unit = {
    var break = false
    while (stackSize > 1 && !break) {
      val n = stackSize - 2
      if (n > 0 && runLen(n - 1) <= runLen(n) + runLen(n + 1)) {
        // Merge the smaller of runLen[n-1] or runLen[n + 1] with runLen[n].
        if (runLen(n - 1) < runLen(n + 1)) {
          // runLen[n-1] is smallest. Merge runLen[n] into runLen[n - 1], leaving
          // runLen[n+1] as the new runLen[n].
          mergeAt(n - 1)
          // n is now stackSize - 1, the top of the stack.
          // Fix for http://b/19493779
          // Because we modified runLen[n - 1] we might have affected invariant 1 as far
          // back as runLen[n - 3]. Check we did not violate it.
          if (n > 2 && runLen(n - 3) <= runLen(n - 2) + runLen(n - 1)) {
            // Avoid leaving invariant 1 still violated on the next loop by also merging
            // runLen[n] into runLen[n - 1].
            mergeAt(n - 1)
            // Now the last three elements in the stack will again be the only elements
            // that might break the invariant and we can loop again safely.
          }
        } else {
          // runLen[n+1] is smallest. Merge runLen[n + 1] into runLen[n].
          mergeAt(n)
        }
      } else if (runLen(n) <= runLen(n + 1)) {
        mergeAt(n)
      } else {
        break = true // Invariant is established
      }
    }
  }

  /**
   * Merges all runs on the stack until only one remains.  This method is
   * called once, to complete the sort.
   */
  private def mergeForceCollapse(): Unit = {
    while (stackSize > 1) {
      var n = stackSize - 2
      if (n > 0 && runLen(n - 1) < runLen(n + 1)) n -= 1
      mergeAt(n)
    }
  }

  /**
   * Merges the two runs at stack indices i and i+1.  Run i must be
   * the penultimate or antepenultimate run on the stack.  In other words,
   * i must be equal to stackSize-2 or stackSize-3.
   *
   * @param i stack index of the first of the two runs to merge
   */
  private def mergeAt(i: Int): Unit = {
    var base1 = runBase(i)
    var len1  = runLen (i)
    val base2 = runBase(i + 1)
    var len2  = runLen (i + 1)

    /*
     * Record the length of the combined runs; if i is the 3rd-last
     * run now, also slide over the last run (which isn't involved
     * in this merge).  The current run (i+1) goes away in any case.
     */
    runLen(i) = len1 + len2
    if (i == stackSize - 3) {
      runBase(i + 1) = runBase(i + 2)
      runLen (i + 1) = runLen (i + 2)
    }
    stackSize -= 1

    /*
     * Find where the first element of run2 goes in run1. Prior elements
     * in run1 can be ignored (because they're already in place).
     */
    val k = gallopRight(a(base2), a, base1, len1, 0, c)
    base1 += k
    len1  -= k

    if (len1 == 0) return

    /*
     * Find where the last element of run1 goes in run2. Subsequent elements
     * in run2 can be ignored (because they're already in place).
     */
    len2 = gallopLeft(a(base1 + len1 - 1), a, base2, len2, len2 - 1, c)

    if (len2 == 0) return

    // Merge remaining runs, using tmp array with min(len1, len2) elements
    if (len1 <= len2)
      mergeLo(base1, len1, base2, len2)
    else
      mergeHi(base1, len1, base2, len2)
  }

  /**
   * Merges two adjacent runs in place, in a stable fashion.  The first
   * element of the first run must be greater than the first element of the
   * second run (a[base1] > a[base2]), and the last element of the first run
   * (a[base1 + len1-1]) must be greater than all elements of the second run.
   *
   * For performance, this method should be called only when len1 &lt;= len2;
   * its twin, mergeHi should be called if len1 &gt;= len2.  (Either method
   * may be called if len1 == len2.)
   *
   * @param base1 index of first element in first run to be merged
   * @param len1_0  length of first run to be merged (must be > 0)
   * @param base2 index of first element in second run to be merged
   *        (must be aBase + aLen)
   * @param len2_0 length of second run to be merged (must be > 0)
   */
  private def mergeLo(base1: Int, len1_0: Int, base2: Int, len2_0: Int): Unit = {
    var len1    = len1_0
    var len2    = len2_0
    // Copy first run into temp array
    val _a      = a    // For performance
    val tmp = ensureCapacity(len1)
    System.arraycopy(_a, base1, tmp, 0, len1)
    var cursor1 = 0     // Indexes into tmp array
    var cursor2 = base2 // Indexes int a
    var dest    = base1 // Indexes int a
    // Move first element of second run and deal with degenerate cases
    _a(dest) = _a(cursor2)
    dest    += 1
    cursor2 += 1
    len2    -= 1
    if (len2 == 0) {
      System.arraycopy(tmp, cursor1, _a, dest, len1)
      return
    }
    if (len1 == 1) {
      System.arraycopy(_a, cursor2, _a, dest, len2)
      _a(dest + len2) = tmp(cursor1) // Last elt of run 1 to end of merge
      return
    }
    val _c = c // Use local variable for performance
    var _minGallop = minGallop // dito

    @tailrec def outer(): Unit = {
      var count1 = 0  // Number of times in a row that first run won
      var count2 = 0  // Number of times in a row that second run won

      /*
       * Do the straightforward thing until (if ever) one run starts
       * winning consistently.
       */
      do {
        if (_c.compare(_a(cursor2), tmp(cursor1)) < 0) {
          _a(dest) = _a(cursor2)
          dest    += 1
          cursor2 += 1
          count2  += 1
          count1   = 0
          len2    -= 1
          if (len2 == 0) {
            return
          }

        } else {
          _a(dest) = tmp(cursor1)
          dest    += 1
          cursor1 += 1
          count1  += 1
          count2   = 0
          len1    -= 1
          if (len1 == 1) {
            return
          }
        }
      } while ((count1 | count2) < _minGallop)
      /*
           * One run is winning so consistently that galloping may be a
           * huge win. So try that, and continue galloping until (if ever)
           * neither run appears to be winning consistently anymore.
           */
      do {
        count1 = gallopRight(_a(cursor2), tmp, cursor1, len1, 0, _c)
        if (count1 != 0) {
          System.arraycopy(tmp, cursor1, _a, dest, count1)
          dest += count1
          cursor1 += count1
          len1 -= count1
          if (len1 <= 1) { // len1 == 1 || len1 == 0
            return
          }
        }
        _a(dest) = _a(cursor2)
        dest += 1
        cursor2 += 1
        len2 -= 1
        if (len2 == 0) {
          return
        }
        count2 = gallopLeft(tmp(cursor1), _a, cursor2, len2, 0, _c)
        if (count2 != 0) {
          System.arraycopy(_a, cursor2, _a, dest, count2)
          dest    += count2
          cursor2 += count2
          len2    -= count2
          if (len2 == 0) {
            return
          }
        }
        _a(dest) = tmp(cursor1)
        dest    += 1
        cursor1 += 1
        len1    -= 1
        if (len1 == 1) {
          return
        }
        _minGallop -= 1
      } while (count1 >= MIN_GALLOP | count2 >= MIN_GALLOP)

      if (_minGallop < 0) _minGallop = 0

      _minGallop += 2  // Penalize for leaving gallop mode
      outer()
    } // End of "outer" loop

    outer()

    minGallop = if (_minGallop < 1) 1 else _minGallop // Write back to field
    if (len1 == 1) {
      System.arraycopy(_a, cursor2, _a, dest, len2)
      _a(dest + len2) = tmp(cursor1) //  Last elt of run 1 to end of merge
    } else if (len1 == 0) {
      throw new IllegalArgumentException(
        "Comparison method violates its general contract!")
    } else {
      System.arraycopy(tmp, cursor1, _a, dest, len1)
    }
  }

  /**
   * Like mergeLo, except that this method should be called only if
   * len1 >= len2; mergeLo should be called if len1 <= len2.  (Either method
   * may be called if len1 == len2.)
   *
   * @param base1 index of first element in first run to be merged
   * @param len1_0 length of first run to be merged (must be > 0)
   * @param base2 index of first element in second run to be merged
   *        (must be aBase + aLen)
   * @param len2_0 length of second run to be merged (must be > 0)
   */
  private def mergeHi(base1: Int, len1_0: Int, base2: Int, len2_0: Int): Unit = {
    var len1 = len1_0
    var len2 = len2_0
    // Copy second run into temp array
    val _a   = a // For performance
    val tmp = ensureCapacity(len2)
    System.arraycopy(_a, base2, tmp, 0, len2)
    var cursor1 = base1 + len1 - 1  // Indexes into a
    var cursor2 = len2 - 1          // Indexes into tmp array
    var dest    = base2 + len2 - 1  // Indexes into a
    // Move last element of first run and deal with degenerate cases
    _a(dest) = _a(cursor1)
    dest    -= 1
    cursor1 -= 1
    len1    -= 1
    if (len1 == 0) {
      System.arraycopy(tmp, 0, _a, dest - (len2 - 1), len2)
      return
    }
    if (len2 == 1) {
      dest -= len1
      cursor1 -= len1
      System.arraycopy(_a, cursor1 + 1, _a, dest + 1, len1)
      _a(dest) = tmp(cursor2)
      return
    }
    val _c = c; // Use local variable for performance
    var _minGallop = minGallop; // dito

    @tailrec def outer(): Unit = {
      var count1 = 0  // Number of times in a row that first run won
      var count2 = 0  // Number of times in a row that second run won
      /*
       * Do the straightforward thing until (if ever) one run
       * appears to win consistently.
       */
      do {
        if (_c.compare(tmp(cursor2), _a(cursor1)) < 0) {
          _a(dest) = _a(cursor1)
          dest    -=1
          cursor1 -= 1
          count1  += 1
          count2   = 0
          len1    -= 1
          if (len1 == 0) {
            return
          }
        } else {
          _a(dest) = tmp(cursor2)
          dest    -= 1
          cursor2 -= 1
          count2  += 1
          count1   = 0
          len2    -= 1
          if (len2 == 1) {
            return
          }
        }
      } while ((count1 | count2) < _minGallop)

      /*
           * One run is winning so consistently that galloping may be a
           * huge win. So try that, and continue galloping until (if ever)
           * neither run appears to be winning consistently anymore.
           */
      do {
        count1 = len1 - gallopRight(tmp(cursor2), _a, base1, len1, len1 - 1, _c)
        if (count1 != 0) {
          dest    -= count1
          cursor1 -= count1
          len1    -= count1
          System.arraycopy(_a, cursor1 + 1, _a, dest + 1, count1)
          if (len1 == 0) {
            return
          }
        }
        _a(dest) = tmp(cursor2)
        dest    -= 1
        cursor2 -= 1
        len2    -= 1
        if (len2 == 1) {
          return
        }
        count2 = len2 - gallopLeft(_a(cursor1), tmp, 0, len2, len2 - 1, _c)
        if (count2 != 0) {
          dest    -= count2
          cursor2 -= count2
          len2    -= count2
          System.arraycopy(tmp, cursor2 + 1, _a, dest + 1, count2)
          if (len2 <= 1) { // len2 == 1 || len2 == 0
            return
          }
        }
        _a(dest) = _a(cursor1)
        dest    -= 1
        cursor1 -= 1
        len1 -= 1
        if (len1 == 0) {
          return
        }
        _minGallop -= 1
      } while (count1 >= MIN_GALLOP | count2 >= MIN_GALLOP)

      if (_minGallop < 0) _minGallop = 0

      _minGallop += 2 // Penalize for leaving gallop mode
      outer()
    } // End of "outer" loop

    outer()

    minGallop = if (_minGallop < 1) 1 else _minGallop; // Write back to field

    if (len2 == 1) {
      dest    -= len1
      cursor1 -= len1
      System.arraycopy(_a, cursor1 + 1, _a, dest + 1, len1)
      _a(dest) = tmp(cursor2); // Move first elt of run2 to front of merge
    } else if (len2 == 0) {
      throw new IllegalArgumentException(
        "Comparison method violates its general contract!")
    } else {
      System.arraycopy(tmp, 0, _a, dest - (len2 - 1), len2)
    }
  }

  /**
   * Ensures that the external array tmp has at least the specified
   * number of elements, increasing its size if necessary.  The size
   * increases exponentially to ensure amortized linear time complexity.
   *
   * @param minCapacity the minimum required capacity of the tmp array
   * @return tmp, whether or not it grew
   */
  private def ensureCapacity(minCapacity: Int): Array[T] = {
    if (tmp.length < minCapacity) {
      // Compute smallest power of 2 > minCapacity
      var newSize = minCapacity
      newSize |= newSize >> 1
      newSize |= newSize >> 2
      newSize |= newSize >> 4
      newSize |= newSize >> 8
      newSize |= newSize >> 16
      newSize += 1
      if (newSize < 0) // Not bloody likely!
        newSize = minCapacity
      else
        newSize = Math.min(newSize, a.length >>> 1)

      tmp = new Array[T](newSize)
    }
    tmp
  }
}
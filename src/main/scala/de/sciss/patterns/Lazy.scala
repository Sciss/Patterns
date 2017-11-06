/*
 *  Lazy.scala
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

object Lazy {
  /** A convenient implementation of the `Lazy` trait for elements which typically expand
    * to streams. This will be typically used for elements which do not directly need to
    * generate streams but rather spawn more graph elements. For the direct generation of
    * `UGen`s, use a subtype of `UGenSource`.
    *
    * The constructor body of this trait will call `Graph.builder.addLazy` to automatically
    * register this element when instantiated.
    *
    * @tparam U   the type to which this element expands, e.g. `Unit` or `UGenInLike`
    */
  trait Expander[+U] extends Lazy {
    // this acts now as a fast unique reference
    @transient final private[this] lazy val ref = new AnyRef

    // ---- constructor ----
    Graph.builder.addLazy(this)

    /** A final implementation of this method which calls `visit` on the builder,
      * checking if this element has already been visited, and if not, will invoke
      * the `expand` method. Therefore it is guaranteed, that the expansion to
      * streams is performed no more than once in the graph expansion.
      */
    final private[patterns] def force(b: StreamGraph.Builder): Unit = expand(b)

    /** A final implementation of this method which looks up the current stream graph
      * builder and then performs the expansion just as `force`, returning the
      * expanded object
      *
      * @return  the expanded object (e.g. `Unit` for a stream with no outputs,
      *          or a single stream, or a group of streams)
      */
    final private[patterns] def expand(implicit b: StreamGraph.Builder): U = b.visit(ref, toStream)

    /** Abstract method which must be implemented by creating the actual `UGen`s
      * during expansion. This method is at most called once during graph
      * expansion
      *
      * @return  the expanded object (depending on the type parameter `U`)
      */
    protected def toStream(implicit b: StreamGraph.Builder): U
  }
}

/** Elements implementing the `Lazy` trait may participate in the building of a
  * `Graph` body. They can be added to the current graph by calling
  * `Graph.builder.addLazy`. Then, when the graph is expanded, the
  * `force` method is called on those registered elements, allowing them
  * to either spawn new graph elements or actually expand to `UGen`s which
  * can be added to the stream graph builder argument.
  *
  * In most cases, lazy elements will expanded to streams, and thus the subtype
  * `Lazy.Expander` is the most convenient way to implement this trait, as it already
  * does most of the logic, and provides for `GE`s `expand` method.
  */
trait Lazy extends Product {
  /** This method is invoked by the `StreamGraph.Builder` instance when a `Graph`
    * is expanded.
    *
    * @param b    the stream graph builder to which expanded `UGen`s or control proxies
    *             may be added.
    */
  private[patterns] def force(b: StreamGraph.Builder): Unit
}
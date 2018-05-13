# Patterns

[![Build Status](https://travis-ci.org/Sciss/Patterns.svg?branch=master)](https://travis-ci.org/Sciss/Patterns)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/patterns_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/patterns_2.11)

## statement

Patterns is a library that brings SuperCollider style patterns to Scala, and ultimately
to [SoundProcesses](https://github.com/Sciss/SoundProcesses)). Patterns are descriptions of streams,
streams are stateful iterators, here implemented with an optional transactional layer.

This project is (C)opyright 2017&ndash;2018 by Hanns Holger Rutz. All rights reserved. This project is released under 
the [GNU Lesser General Public License](https://raw.github.com/Sciss/Patterns/master/LICENSE) v2.1+ and comes 
with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

## requirements / installation

This project builds against Scala 2.12, 2.11 using [sbt](http://www.scala-sbt.org/).

To link to it:

    libraryDependencies += "de.sciss" %% "patterns" % v

The current version `v` is `"0.3.0"`.

There are two sub-modules which can be linked to separately:

- `patterns-core` is the foundation, it works with the mutable (non-transactional) `Base` system
- `patterns-lucre` adds support for [Lucre](https://github.com/Sciss/Lucre/) type transactional systems
  (what is used in SoundProcesses)

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

## getting started

Useful links:

- [Patterns in SuperCollider](http://doc.sccode.org/Tutorials/A-Practical-Guide/PG_01_Introduction.html)

Example:

```scala
import de.sciss.patterns._, graph._

val g = Graph {
  Pat.loop(3) {
    Brown(1, 100, 3).take(4)
  }
}

implicit val ctx = Context()
println(g.expand.toList)
// e.g. List(45, 42, 43, 41,   88, 85, 88, 91,   19, 21, 21, 23)
```

Note the following things:

- implicit conversions for using constants as patterns come from `de.sciss.patterns._`
- all pattern classes (sub-classes of `Pat`) are defined in package `de.sciss.patterns.graph`
- you should always wrap a pattern expression in a `Graph { }` block. This ensures that you
  can use control structures such as `map` and `flatMap`.
- streams are implemented without co-routines; we provide some mechanism for commonly used
  imperative statements, such as `loop` and `while`; here we use `Pat.loop(n) { }` which can
  be understood as `(0 until n).flatMap { ... }`, i.e. it builds the inner pattern three times
  and concatenates the results.
- because we have dedicated namespace, there is no `P` prefix in class-names, thus it is
  `Brown` and not `Pbrown`.
- we treat patterns similar to Scala collections, so many of the same operations are defined,
  such as `drop` and `take` (corresponding with `drop` and `keep` in SuperCollider).
- a pattern is expanded to a `Stream` using `expand`. That method takes an implicit context
  and transaction. Here we use a `Plain` context which does not use transactions. We make
  the context implicitly available, so `expand` works. If you are interested in persistent
  streams, have a look at the `StreamSerializationSpec` test source.
- to get values from a stream, iterate using `hasNext` and `next()`, or call `toIterator`, 
  `toList` and `toVector`.
  
For a more complex example, see the `RonTuplePure` test source code.
For further questions, consult the source code, API docs (`sbt doc`), or visit 
the [Gitter chat room](https://gitter.im/Sciss/Patterns).

## notes for implementing new patterns

- there should a `Pat` Element in `graph`
- there should be a corresponding `Stream` Element in `stream`
- there should be a unit test verifying correct production of values
- there should be a unit test verifying correct reaction to `reset`
- there should be a unit test verifying correct serialization

## to-do / things to explore

- stream caching in order to avoid excessive forks at the expansion. We would cache one or two
  elements, so that stuff such as `a + a` would only require a single a expansion of `a` (and
  of the inputs of `a` accordingly). This would hopefully speed things up a bit.
- multi-channel-expansion, while principally supported, is not well tested, with respect to
  correctness and type-class / DSL support
  

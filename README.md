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

This project currently builds against Scala 2.12, 2.11 using [sbt](http://www.scala-sbt.org/).

To link to it:

    libraryDependencies += "de.sciss" %% "patterns" % v

The current version `v` is `"0.1.0"`

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

## documentation

Useful links:

- [Patterns in SuperCollider](http://doc.sccode.org/Tutorials/A-Practical-Guide/PG_01_Introduction.html)

## notes for implementing new patterns

- there should a `Pat` Element in `graph`
- there should be a corresponding `Stream` Element in `stream`
- there should be a unit test verifying correct production of values
- there should be a unit test verifying correct reaction to `reset`
- there should be a unit test verifying correct serialization

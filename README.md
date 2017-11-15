# Patterns

[![Build Status](https://travis-ci.org/Sciss/Patterns.svg?branch=master)](https://travis-ci.org/Sciss/Patterns)

## statement

Patterns is an ongoing experiment to see how SuperCollider's patterns system could be translated to Scala (and ultimately, [SoundProcesses](https://github.com/Sciss/SoundProcesses)).
It is (C)opyright 2017 by Hanns Holger Rutz. All rights reserved. This project is released under the [GNU Lesser General Public License](https://raw.github.com/Sciss/Patterns/master/LICENSE) v2.1+ and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

## requirements / installation

This project currently builds against Scala 2.12, 2.11 using sbt. For convenience, the [sbt-extras](https://github.com/paulp/sbt-extras)
script by Paul Phillips (BSD-3-Clause) is included.

To link to it:

    libraryDependencies += "de.sciss" %% "patterns" % v

The current version `v` is `"0.1.0-SNAPSHOT"`

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

## documentation

Useful links:

- [Patterns in SuperCollider](http://doc.sccode.org/Tutorials/A-Practical-Guide/PG_01_Introduction.html)

Current testing demo: `sbt test:run` (have have SuperCollider installed and `scsynth` on the `PATH`!)

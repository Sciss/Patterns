package de.sciss.patterns

import de.sciss.file._
import de.sciss.lucre.Durable
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.synth.proc.{Pattern, SoundProcesses}
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec

trait DurableSpec extends FixtureAnyFlatSpec {
  type S = Durable
  type T = Durable.Txn
  type FixtureParam = S

  SoundProcesses.init()
  Pattern       .init()

  final def withFixture(test: OneArgTest): Outcome = {
    val dir = File.createTemp("sleepycat_", "db")
    dir.delete()
    val system = Durable(BerkeleyDB.factory(dir))
    try {
      test(system)
    }
    finally {
      system.close()
      deleteRecursive(dir)
    }
  }

  private def deleteRecursive(dir: File): Unit = {
    val (subDir, files) = dir.children.partition(_.isDirectory)
    subDir.foreach(deleteRecursive)
    files .foreach(_.delete()) // (f => if (!f.delete()) f.deleteOnExit())
    dir.delete()
  }
}

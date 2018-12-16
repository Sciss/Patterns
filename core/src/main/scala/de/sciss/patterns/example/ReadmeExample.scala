package de.sciss.patterns.example

object ReadmeExample {
  def main(args: Array[String]): Unit = new ReadmeExample
}
class ReadmeExample {
  import de.sciss.patterns._, graph._

  val g = Graph {
    Pat.loop(3) {
      Brown(1, 100, 3).take(4)
    }
  }

  implicit val ctx = Context()
  println(g.expand.toList)
  // e.g. List(45, 42, 43, 41,   88, 85, 88, 91,   19, 21, 21, 23)
}
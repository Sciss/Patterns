//package de.sciss.patterns
//
//import de.sciss.patterns.graph._
//
//class ListPatternsSpec extends PatSpec {
//  "A Pseq" should "produce the expected output for the help file examples" in {
//    // from help file
//    val pat1 = Pseq(Seq(Pat(1), Pat(2), Pat(3)), 2)  // repeat twice
//    eval(pat1) shouldBe Seq(1, 2, 3, 1, 2, 3)
//
//    val pat2 = Pseq(Seq(Pat(1), Pat(2), Pat(3), Pat(4)), 3, 2)  // repeat 3, offset 2
//    eval(pat2) shouldBe Seq(3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2)
//  }
//
//  "Pseq and Stutter" should "produce the expected output" in {
//    val pat1 = Stutter(Pseq(Seq(Pat(1), Pat(2), Pat(3)), 66), n = 2)
//    eval(pat1, 9) shouldBe Seq(1, 1, 2, 2, 3, 3, 1, 1, 2)
//  }
//
////  it should "produce the expected output for a 'nested' input" in {
////    val pat = Pseq(Seq(60, 61, 62), 2, Seq(1, 2))
////    eval(pat) shouldBe Seq(Seq(61, 62), Seq(62, 60), Seq(60, 61), Seq(61, 62), Seq(62, 60), Seq(60, 61))
////  }
//}

package chord

import note.Note
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.TableFor2
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Unit tests for testing the abstract logic of a triad.
  */
abstract class ATriadTest() extends FunSuite with ScalaCheckPropertyChecks {
  val chordGenerator: Gen[Chord]
  val chordNotes: TableFor2[String, List[String]]
  val expectedChordName: String => String
  val chordApplyFunction: String => Option[Chord]

  val firstToSecondDistance: (Note => Int)
  val secondToThirdDistance: (Note => Int)
  val firstToThirdDistance: (Note => Int)

  test("the triad should have the expected intervals") {
    forAll(chordGenerator) {
      chord =>
        val root = chord.notes.head
        val second = chord.notes(1)
        val third = chord.notes(2)

        assert(root.distance(second) == firstToSecondDistance(root))
        assert(second.distance(third) == secondToThirdDistance(second))
        assert(root.distance(third) == firstToThirdDistance(root))
    }
  }

  test("suspended second should have the expected chord name") {
    forAll(chordGenerator) {
      chord => chord.toString == expectedChordName(chord.tonic)
    }
  }

  test("the triad should correctly return the expected notes") {
    forAll(chordNotes) { (tonic: String, notes: List[String]) =>
      val note = chordApplyFunction(tonic).get
      assert(note.notes.map(_.note) == notes)
    }
  }

  test("Invalid chord names should return None") {
    val regex = "^[A-G](?:#*|b*)$".r
    val invalidChordGenerator: Gen[String] = for {
      invalidString <- arbitrary[String].filterNot(s => regex.matches(s))
    } yield invalidString

    forAll(invalidChordGenerator) { s =>
      assert(chordApplyFunction(s).isEmpty)
    }
  }
}

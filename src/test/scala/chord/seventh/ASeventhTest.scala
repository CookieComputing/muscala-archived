package chord.seventh

import chord.Chord
import note.Note
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.TableFor2
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Unit tests for abstract seventh tests
  */
abstract class ASeventhTest extends FunSuite with ScalaCheckPropertyChecks{
  val chordGenerator: Gen[SeventhChord]
  val chordNotes: TableFor2[String, List[String]]
  val expectedChordName: String => String
  val chordApplyFunction: String => Option[SeventhChord]
  val expectedTriad: String => Chord

  val expectedRootIntervals: SeventhChord => Boolean
  val expectedConsecutiveThirdIntervals: SeventhChord => Boolean

  test("the intervals from root should be correct") {
    forAll(chordGenerator) {
      chord =>
        assert(expectedRootIntervals(chord))
    }
  }

  test("the quality of consecutive thirds should be correct") {
    forAll(chordGenerator) {
      chord =>
        assert(expectedConsecutiveThirdIntervals(chord))
    }
  }

  test("the seventh chord should have the expected name") {
    forAll(chordGenerator) {
      chord =>
        assert(chord.toString == expectedChordName(chord.tonic))
    }
  }

  test("the seventh chord should return the expected notes") {
    forAll(chordNotes) { (tonic: String, notes: List[String]) =>
      val chord = chordApplyFunction(tonic).get
      assert(chord.notes.map(_.note) == notes)
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

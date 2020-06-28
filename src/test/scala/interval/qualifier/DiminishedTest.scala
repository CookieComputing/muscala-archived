package interval.qualifier

import helpers.{NoteTesting, PropertyTesting}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

case class DiminishedTest() extends FunSuite with ScalaCheckPropertyChecks {
  test("diminished fourth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "Db"),
        ("B", "Eb"),
        ("C", "Fb"),
        ("D", "Gb"),
        ("E", "Ab"),
        ("F", "A"),
        ("G", "Cb")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.diminished.fourth.toString == expectedNote.toString)
      }
  }

  test("diminished fourth notes are a distance of 4 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.diminished.fourth) == 4)
    }
  }

  test("diminished fifth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "Eb"),
        ("B", "F"),
        ("C", "Gb"),
        ("D", "Ab"),
        ("E", "Bb"),
        ("F", "Cb"),
        ("G", "Db")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.diminished.fifth.toString == expectedNote.toString)
      }
  }

  test("diminished fifth notes are a distance of 6 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.diminished.fifth) == 6)
    }
  }

  test("diminished seventh natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "Gb"),
        ("B", "Ab"),
        ("C", "A"), // Note that this is a case where the naive solution for note correction fails.
        ("D", "Cb"),
        ("E", "Db"),
        ("F", "D"), // Note that this is a case where the naive solution for note correction fails.
        ("G", "Fb")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.diminished.seventh.toString == expectedNote.toString)
      }
  }

  test("diminished seventh notes are a distance of 9 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.diminished.seventh) == 9)
    }
  }
}

package interval.qualifier

import helpers.{NoteTesting, PropertyTesting}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Represents unit tests for the minor interval qualifier
  */
class MinorTest extends FunSuite with ScalaCheckPropertyChecks {
  test("minor second natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "A#"),
        ("B", "C"),
        ("C", "C#"),
        ("D", "D#"),
        ("E", "F"),
        ("F", "F#"),
        ("G", "G#")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(
            actualNote.minor.second.applyAccidentals.toString == expectedNote.toString
          )
      }
  }

  test("minor second notes are a distance of 1 half step away") {
    forAll(PropertyTesting.noteGen) {
      note =>
      assert(note.distance(note.minor.second) == 1)
    }
  }

  test("minor third natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "C"),
        ("B", "D"),
        ("C", "D#"),
        ("D", "F"),
        ("E", "G"),
        ("F", "G#"),
        ("G", "A#")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.minor.third.toString == expectedNote.toString)
      }
  }

  test("minor third notes are a distance of 3 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.minor.third) == 3)
    }
  }

  test("minor sixth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "F"),
        ("B", "G"),
        ("C", "G#"),
        ("D", "A#"),
        ("E", "C"),
        ("F", "C#"),
        ("G", "D#")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.minor.sixth.toString == expectedNote.toString)
      }
  }

  test("minor sixth notes are a distance of 8 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.minor.sixth) == 8)
    }
  }

  test("minor seventh natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "G"),
        ("B", "A"),
        ("C", "A#"),
        ("D", "C"),
        ("E", "D"),
        ("F", "D#"),
        ("G", "F")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.minor.seventh.toString == expectedNote.toString)
      }
  }

  test("minor seventh notes are a distance of 10 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.minor.seventh) == 10)
    }
  }
}

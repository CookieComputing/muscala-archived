package interval.qualifier

import helpers.{NoteTesting, PropertyTesting}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class AugmentedTest extends FunSuite with ScalaCheckPropertyChecks {
  test("augmented fourth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "D#"),
        ("B", "E#"),
        ("C", "F#"),
        ("D", "G#"),
        ("E", "A#"),
        ("F", "A##"),
        ("G", "C#")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.augmented.fourth.toString == expectedNote.toString)
      }
  }

  test("augmented fourth notes are a distance of 6 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.augmented.fourth) == 6)
    }
  }

  test("augmented fifth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "E#"),
        ("B", "F##"),
        ("C", "G#"),
        ("D", "A#"),
        ("E", "B#"),
        ("F", "C#"),
        ("G", "D#")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.augmented.fifth.toString == expectedNote.toString)
      }
  }

  test("augmented fifth notes are a distance of 8 half steps away") {
    forAll(PropertyTesting.noteGen) {
    note =>
        assert(note.distance(note.augmented.fifth) == 8)
    }
  }
}

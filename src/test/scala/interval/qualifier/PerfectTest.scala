package interval.qualifier

import helpers.{NoteTesting, PropertyTesting}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Represents unit tests for the perfect interval qualifier
  */
class PerfectTest extends FunSuite with ScalaCheckPropertyChecks {
  test("perfect fourth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "D"),
        ("B", "E"),
        ("C", "F"),
        ("D", "G"),
        ("E", "A"),
        ("F", "A#"), // this is an exception, since absolute intervals count in sharps as opposed to flats
        // refer to diatonic key intervals for a more accurate depiction of intervals
        ("G", "C")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.perfect.fourth.toString == expectedNote.toString)
      }
  }

  test("perfect fourth notes are a distance of 5 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.perfect.fourth) == 5)
    }
  }

  test("perfect fifth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "E"),
        ("B", "F#"),
        ("C", "G"),
        ("D", "A"),
        ("E", "B"),
        ("F", "C"),
        ("G", "D")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.perfect.fifth.toString == expectedNote.toString)
      }
  }

  test("perfect fifth notes are a distance of 7 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.perfect.fifth) == 7)
    }
  }

  test("perfect octave natural note should work as expected") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note)
      }
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.perfect.octave.toString == expectedNote.toString)
      }
  }

  test("perfect octave notes are a distance of 12 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.perfect.octave) == 12)
    }
  }
}

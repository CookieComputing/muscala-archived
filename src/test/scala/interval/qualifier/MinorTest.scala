package interval.qualifier

import helpers.NoteTesting
import org.scalatest.FunSuite

/**
  * Represents unit tests for the minor interval qualifier
  */
class MinorTest extends FunSuite {
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.minor.second)
      }
      .map {
        case (note, minorSecond) =>
          assert(note.distance(minorSecond) == 1)
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.minor.third)
      }
      .map {
        case (note, minorThird) =>
          assert(note.distance(minorThird) == 3)
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.minor.sixth)
      }
      .map {
        case (note, minorSixth) =>
          assert(note.distance(minorSixth) == 8)
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.minor.seventh)
      }
      .map {
        case (note, minorSixth) =>
          assert(note.distance(minorSixth) == 10)
      }
  }
}

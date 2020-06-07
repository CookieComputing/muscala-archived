package interval.qualifier

import helpers.NoteTesting
import org.scalatest.FunSuite

/**
  * Represents unit tests for the major interval qualifier
  */
class MajorTest extends FunSuite {
  test("major second natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "B"),
        ("B", "C#"),
        ("C", "D"),
        ("D", "E"),
        ("E", "F#"),
        ("F", "G"),
        ("G", "A")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.major.second.toString == expectedNote.toString)
      }
  }

  test("major second notes are a distance of 1 whole steps away") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.major.second)
      }
      .map {
        case (note, majorSecond) =>
          assert(note.distance(majorSecond) == 2)
      }
  }

  test("major third natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "C#"),
        ("B", "D#"),
        ("C", "E"),
        ("D", "F#"),
        ("E", "G#"),
        ("F", "A"),
        ("G", "B")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.major.third.toString == expectedNote.toString)
      }
  }

  test("major third notes are a distance of 2 whole steps away") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.major.third)
      }
      .map {
        case (note, majorThird) =>
          assert(note.distance(majorThird) == 4)
      }
  }

  test("major sixth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "F#"),
        ("B", "G#"),
        ("C", "A"),
        ("D", "B"),
        ("E", "C#"),
        ("F", "D"),
        ("G", "E")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.major.sixth.toString == expectedNote.toString)
      }
  }

  test("major sixth notes are a distance of 9 half steps away") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.major.sixth)
      }
      .map {
        case (note, majorSixth) =>
          assert(note.distance(majorSixth) == 9)
      }
  }

  test("major seventh natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "G#"),
        ("B", "A#"),
        ("C", "B"),
        ("D", "C#"),
        ("E", "D#"),
        ("F", "E"),
        ("G", "F#")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.major.seventh.toString == expectedNote.toString)
      }
  }

  test("major seventh notes are a distance of 11 half steps away") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.major.seventh)
      }
      .map {
        case (note, majorSixth) =>
          assert(note.distance(majorSixth) == 11)
      }
  }
}

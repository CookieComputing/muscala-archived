package interval.qualifier.absolute

import helpers.NoteTesting
import interval.qualifier.absolute.Imports._
import org.scalatest.FunSuite

/**
  * Represents unit tests for the perfect interval qualifier
  */
class PerfectTest extends FunSuite {
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.perfect.fourth)
      }
      .map {
        case (note, perfectFourth) =>
          assert(note.distance(perfectFourth) == 5)
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.perfect.fifth)
      }
      .map {
        case (note, perfectFifth) =>
          assert(note.distance(perfectFifth) == 7)
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.perfect.octave)
      }
      .map {
        case (note, perfectOctave) =>
          assert(note.distance(perfectOctave) == 12)
      }
  }

  test("perfect octave notes are exactly one octave apart") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.perfect.octave)
      }
      .map {
        case (note, perfectOctave) =>
          assert(perfectOctave.octave == note.octave + 1)
      }
  }
}

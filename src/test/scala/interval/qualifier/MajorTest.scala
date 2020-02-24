package interval.qualifier

import helpers.NoteTesting
import note.Note
import org.scalatest.FunSuite

/**
  * Represents unit tests for the major interval qualifer
  */
class MajorTest extends FunSuite {
  test("major third natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "C#"),
        ("B", "D#"),
        ("C", "E"),
        ("D", "F#"),
        ("E", "G#"),
        ("F", "A"),
        ("G", "B"),
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.major.third.toString == expectedNote.toString)
      }
  }

  test("major third notes are a distance of 2 whole steps away") {
    NoteTesting
      .toNoteSeq(
        "A",
        "A#",
        "Bb",
        "B",
        "C",
        "C#",
        "Db",
        "D",
        "D#",
        "Eb",
        "E",
        "F",
        "F#",
        "Gb",
        "G",
        "G#",
        "Ab"
      )
      .map { note =>
        (note, note.major.third)
      }
      .map {
        case (note, majorThird) =>
          assert(note.distance(majorThird) == 4)
      }
  }
}

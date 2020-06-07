package interval.qualifier

import helpers.NoteTesting
import org.scalatest.FunSuite

class AugmentedTest extends FunSuite {
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.augmented.fourth)
      }
      .map {
        case (note, augmentedFourth) =>
          assert(note.distance(augmentedFourth) == 6)
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
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.augmented.fifth)
      }
      .map {
        case (note, augmentedFifth) =>
          assert(note.distance(augmentedFifth) == 8)
      }
  }
}

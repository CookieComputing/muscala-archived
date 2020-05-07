package interval.movement.diatonic

import helpers.NoteTesting
import key.{Key, MajorKey}
import org.scalatest.FunSuite
import interval.movement.diatonic.Imports._

class HalfStepTest extends FunSuite {
  /*
  test("Notes in the key of C are correctly adjusted") {
    implicit val key: Key = MajorKey.C

    NoteTesting
      .toNoteTupleSeq(
        ("A", "A#"),
        ("B", "C"),
        ("C", "C#"),
        ("D", "D#"),
        ("E", "F"),
        ("F", "F#"),
        ("G", "G#")
      ).map {
      case (actualNote, expectedNote) =>
        assert(actualNote.halfStep.up.note == expectedNote.note)
    }

    NoteTesting
      .toNoteTupleSeq(
        ("A", "Ab"),
        ("B", "Bb"),
        ("C", "B"),
        ("D", "Db"),
        ("E", "Eb"),
        ("F", "E"),
        ("G", "Gb")
      ).map {
      case (actualNote, expectedNote) =>
        assert(actualNote.halfStep.down.note == expectedNote.note)
    }
  }

  test("Notes in the key of B are correctly adjusted") {
    implicit val key: Key = MajorKey.B
    // TODO: Ugh this is painful, how does mingus solve this with keys?
    NoteTesting
      .toNoteTupleSeq(
        ("A", "A#"),
        ("B", "C#b"),
        ("C", "C#"),
        ("D", "D#"),
        ("E", "F#b"),
        ("F", "F#"),
        ("G", "G#")
      ).map {
      case (actualNote, expectedNote) =>
        assert(actualNote.halfStep.up.note == expectedNote.note)
    }

    NoteTesting
      .toNoteTupleSeq(
        ("A", "G#"),
        ("B", "A#"),
        ("C", "B"),
        ("D", "C#"),
        ("E", "Eb"),
        ("F", "E"),
        ("G", "F#")
      ).map {
      case (actualNote, expectedNote) =>
        assert(actualNote.halfStep.down.note == expectedNote.note)
    }
  }

   */
}

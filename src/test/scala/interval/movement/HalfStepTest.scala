package interval.movement

import helpers.NoteTesting
import interval.movement
import note.Note
import org.scalatest.FunSuite
import interval.movement.Imports._

/**
  * Unit tests for half step movements.
  */
class HalfStepTest extends FunSuite {
  test(
    "Movements from natural notes up should result in accidentals being created"
  ) {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "A#"),
        ("C", "C#"),
        ("D", "D#"),
        ("E", "E#"),
        ("F", "F#"),
        ("G", "G#")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(
            HalfStep(actualNote).up.applyAccidentals == expectedNote.applyAccidentals
          )
      }

    // special edge case
    assert(
      movement
        .HalfStep(Note("B").get)
        .up
        .applyAccidentals == Note("C", 5).get.applyAccidentals
    )
  }

  test(
    "Movements from accidentals up should increment the sharp number or remove a flat"
  ) {
    // One caveat in this test is that we are simply applying sharp and/or flat, which will still retain
    // the sharps in an accidental
    NoteTesting
      .toNoteTupleSeq(
        ("A#", "B"),
        ("Bb", "B"),
        ("C#", "D"),
        ("Db", "D"),
        ("D#", "E"),
        ("Eb", "E"),
        ("F#", "G"),
        ("Gb", "G")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(
            movement.HalfStep(actualNote).up.applyAccidentals == expectedNote
          )
      }
  }

  test(
    "Movements from accidental notes down should result in natural notes being created"
  ) {
    NoteTesting
      .toNoteTupleSeq(
        ("G#", "G"),
        ("F#", "F"),
        ("E#", "E"),
        ("D#", "D"),
        ("C#", "C"),
        ("B#", "B"),
        ("A#", "A")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(movement.HalfStep(actualNote).down == expectedNote)
      }
  }

  test(
    "Movements from natural notes down should increment the flat number or remove a sharp"
  ) {
    // One caveat in this test is that we are simply applying sharp and/or flat, which will still retain
    // the sharps in an accidental
    NoteTesting
      .toNoteTupleSeq(
        ("G#", "G"),
        ("G", "Gb"),
        ("F#", "F"),
        ("E#", "E"),
        ("E", "Eb"),
        ("D#", "D"),
        ("D", "Db"),
        ("C#", "C"),
        ("B#", "B"),
        ("B", "Bb"),
        ("A#", "A"),
        ("A", "Ab")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(
            movement.HalfStep(actualNote).down.applyAccidentals == expectedNote
          )
      }
  }

  test("A note should be extractable from a half step") {
    Note.A.halfStep match {
      case HalfStep(note) => assert(note == Note.A)
      case _ =>
        assert(condition = false, "expected half step when calling halfStep() method")
    }
  }
}

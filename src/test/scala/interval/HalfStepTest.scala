package interval

import helpers.NoteTesting
import note.Note
import org.scalatest.FunSuite

/**
 * Unit tests for half step movements.
 */
class HalfStepTest extends FunSuite {
  test("Movements from natural notes up should result in accidentals being created") {
    NoteTesting.toNoteTupleSeq(List(
      ("A", "A#"),
      ("C", "C#"),
      ("D", "D#"),
      ("E", "E#"),
      ("F", "F#"),
      ("G", "G#"))
    ).map { case (actualNote, expectedNote) =>
      assert(HalfStep(actualNote).up.applyAccidentals == expectedNote.applyAccidentals)
    }

    // special edge case
    assert(HalfStep(Note("B").get).up.applyAccidentals == Note("C", 5).get.applyAccidentals)
  }

  test("Movements from accidentals up should increment the sharp number or remove a flat") {
    // One caveat in this test is that we are simply applying sharp and/or flat, which will still retain
    // the sharps in an accidental
    NoteTesting.toNoteTupleSeq(List(
      ("A#", "B"),
      ("Bb", "B"),
      ("C#", "D"),
      ("Db", "D"),
      ("D#", "E"),
      ("Eb", "E"),
      ("F#", "G"),
      ("Gb", "G"))
    ).map { case (actualNote, expectedNote) =>
      assert(HalfStep(actualNote).up.applyAccidentals == expectedNote)
    }
  }

  test("Movements from accidental notes down should result in natural notes being created") {
    NoteTesting.toNoteTupleSeq(List(
      ("G#", "G"),
      ("F#", "F"),
      ("E#", "E"),
      ("D#", "D"),
      ("C#", "C"),
      ("B#", "B"),
      ("A#", "A"))
    ).map { case (actualNote, expectedNote) =>
      assert(HalfStep(actualNote).down == expectedNote)
    }
  }

  test("Movements from natural notes down should increment the flat number or remove a sharp") {
    // One caveat in this test is that we are simply applying sharp and/or flat, which will still retain
    // the sharps in an accidental
    NoteTesting.toNoteTupleSeq(List(
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
      ("A", "Ab"))
    ).map { case (actualNote, expectedNote) =>
      assert(HalfStep(actualNote).down.applyAccidentals == expectedNote)
    }
  }
}

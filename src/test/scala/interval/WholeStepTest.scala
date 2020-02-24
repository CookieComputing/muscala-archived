package interval

import helpers.NoteTesting
import note.Note
import org.scalatest.FunSuite

/**
 * Represents unit tests for whole step movements
 */
class WholeStepTest extends FunSuite {
  test("Movement up from natural notes should work properly") {
    NoteTesting.toNoteTupleSeq(
      List(("A", "B"),
        ("C", "D"),
        ("D", "E"),
        ("F", "G"),
        ("G", "A")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).up == expectedNote)}
  }

  test("Movement up from flats should retain flat value") {
    NoteTesting.toNoteTupleSeq(
      List(("Ab", "Bb"),
        ("Cb", "Db"),
        ("Db", "Eb"),
        ("Fb", "Gb"),
        ("Gb", "Ab")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).up == expectedNote)}
  }

  test("Movement up from sharps should retain sharp value") {
    NoteTesting.toNoteTupleSeq(
      List(("A#", "B#"),
        ("C#", "D#"),
        ("D#", "E#"),
        ("F#", "G#"),
        ("G#", "A#")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).up == expectedNote)}
  }

  test("Double accidentals moving up should retain their expected values") {
    NoteTesting.toNoteTupleSeq(
      List(("Abb", "Bbb"),
        ("Cbb", "Dbb"),
        ("Dbb", "Ebb"),
        ("Fbb", "Gbb"),
        ("Gbb", "Abb")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).up == expectedNote)}

    NoteTesting.toNoteTupleSeq(
      List(("A##", "B##"),
        ("C##", "D##"),
        ("D##", "E##"),
        ("F##", "G##"),
        ("G##", "A##")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).up == expectedNote)}
  }

  test("Edge case notes going up should begin dropping accidentals when necessary") {
    List((Note("Bb"), Note("C", 5)), (Note("Eb"), Note("F")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).up == expectedNote)}
  }

  test("Movement down from natural notes should work properly") {
    NoteTesting.toNoteTupleSeq(
      List(("B", "A"),
        ("E", "D"),
        ("D", "C"),
        ("G", "F"),
        ("A", "G")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).down == expectedNote)}
  }

  test("Movement down from flats should retain flat value") {
    NoteTesting.toNoteTupleSeq(
      List(("Bb", "Ab"),
        ("Eb", "Db"),
        ("Db", "Cb"),
        ("Gb", "Fb"),
        ("Ab", "Gb")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).down == expectedNote)}
  }

  test("Movement down from sharps should retain sharp value") {
    NoteTesting.toNoteTupleSeq(
      List(("B#", "A#"),
        ("E#", "D#"),
        ("D#", "C#"),
        ("G#", "F#"),
        ("A#", "G#")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).down == expectedNote)}
  }

  test("Double accidentals moving down should retain their expected values") {
    NoteTesting.toNoteTupleSeq(
      List(("Bbb", "Abb"),
        ("Ebb", "Dbb"),
        ("Dbb", "Cbb"),
        ("Gbb", "Fbb"),
        ("Abb", "Gbb")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).down == expectedNote)}

    NoteTesting.toNoteTupleSeq(
      List(("B##", "A##"),
        ("E##", "D##"),
        ("D##", "C##"),
        ("G##", "F##"),
        ("A##", "G##")))
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).down == expectedNote)}
  }

  test("Edge case notes going down should begin dropping accidentals when necessary") {
    List((Note("C#", 5), Note("B", 4)), (Note("F#"), Note("E")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(WholeStep(actualNote).down == expectedNote)}
  }
}

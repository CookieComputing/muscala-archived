package interval

import note.Note
import org.scalatest.FunSuite

/**
 * Represents unit tests for whole step movements
 */
class WholeStepMovementTest extends FunSuite {
  test("Movement up from natural notes should work properly") {
    List((Note("A"), Note("B")), (Note("C"), Note("D")),
      (Note("D"), Note("E")), (Note("F"), Note("G")), (Note("G"), Note("A")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).up == expectedNote)}
  }

  test("Movement up from flats should retain flat value") {
    List((Note("Ab"), Note("Bb")), (Note("Cb"), Note("Db")),
      (Note("Db"), Note("Eb")), (Note("Fb"), Note("Gb")), (Note("Gb"), Note("Ab")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).up == expectedNote)}
  }

  test("Movement up from sharps should retain sharp value") {
    List((Note("A#"), Note("B#")), (Note("C#"), Note("D#")),
      (Note("D#"), Note("E#")), (Note("F#"), Note("G#")), (Note("G#"), Note("A#")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).up == expectedNote)}
  }

  test("Double accidentals moving up should retain their expected values") {
    List((Note("Abb"), Note("Bbb")), (Note("Cbb"), Note("Dbb")),
      (Note("Dbb"), Note("Ebb")), (Note("Fbb"), Note("Gbb")), (Note("Gbb"), Note("Abb")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).up == expectedNote)}

    List((Note("A##"), Note("B##")), (Note("C##"), Note("D##")),
      (Note("D##"), Note("E##")), (Note("F##"), Note("G##")), (Note("G##"), Note("A##")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).up == expectedNote)}
  }

  test("Edge case notes going up should begin dropping accidentals when necessary") {
    List((Note("Bb"), Note("C", 5)), (Note("Eb"), Note("F")))
      .map { tuple => (tuple._1.get, tuple._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).up == expectedNote)}
  }

  test("Movement down from natural notes should work properly") {
    List((Note("B"), Note("A")), (Note("D"), Note("C")),
      (Note("E"), Note("D")), (Note("G"), Note("F")), (Note("A"), Note("G")))
      .map { tdownle => (tdownle._1.get, tdownle._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).down == expectedNote)}
  }

  test("Movement down from flats should retain flat value") {
    List((Note("Bb"), Note("Ab")), (Note("Db"), Note("Cb")),
      (Note("Eb"), Note("Db")), (Note("Gb"), Note("Fb")), (Note("Ab"), Note("Gb")))
      .map { tdownle => (tdownle._1.get, tdownle._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).down == expectedNote)}
  }

  test("Movement down from sharps should retain sharp value") {
    List((Note("B#"), Note("A#")), (Note("D#"), Note("C#")),
      (Note("E#"), Note("D#")), (Note("G#"), Note("F#")), (Note("A#"), Note("G#")))
      .map { tdownle => (tdownle._1.get, tdownle._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).down == expectedNote)}
  }

  test("Double accidentals moving down should retain their expected values") {
    List((Note("Bbb"), Note("Abb")), (Note("Dbb"), Note("Cbb")),
      (Note("Ebb"), Note("Dbb")), (Note("Gbb"), Note("Fbb")), (Note("Abb"), Note("Gbb")))
      .map { tdownle => (tdownle._1.get, tdownle._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).down == expectedNote)}

    List((Note("B##"), Note("A##")), (Note("D##"), Note("C##")),
      (Note("E##"), Note("D##")), (Note("G##"), Note("F##")), (Note("A##"), Note("G##")))
      .map { tdownle => (tdownle._1.get, tdownle._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).down == expectedNote)}
  }

  test("Edge case notes going down should begin dropping accidentals when necessary") {
    List((Note("C#", 5), Note("B", 4)), (Note("F#"), Note("E")))
      .map { tdownle => (tdownle._1.get, tdownle._2.get) }
      .map { case (actualNote, expectedNote) =>
        assert(new WholeStepMovement(actualNote).down == expectedNote)}
  }
}

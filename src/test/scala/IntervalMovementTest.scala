import note.Note
import org.scalatest.FunSuite

// Unit tests fore the whole step and half step classes
class IntervalMovementTest extends FunSuite {
  test("A whole step should correctly be a whole step") {
    List((Note("C"), Note("D")), (Note("C#"), Note("D#")),
      (Note("D"), (Note("E"))), (Note("Ab"), Note("A#")),
      (Note("Eb"), Note("F")), (Note("G#"), Note("A#")))
      .map {tuple => (tuple._1.get.wholeStep.up, tuple._2.get)}
      .map {case (actualNote, expectedNote)
      => assert(actualNote == expectedNote)}

    List((Note("D"), Note("C")), (Note("A"), Note("G")),
      (Note("Bb"), Note("Ab")), (Note("A#"), Note("Ab")),
      (Note("F"), Note("Eb")), (Note("Eb"), Note("Db")))
      .map {tuple => (tuple._1.get.wholeStep.down, tuple._2.get)}
      .map {case (actualNote, expectedNote)
      => assert(actualNote == expectedNote)}
  }

  test("A half step should correctly be a half step") {
    List((Note("C"), Note("C#")), (Note("C#"), Note("D")),
      (Note("D"), (Note("D#"))), (Note("Ab"), Note("A")),
      (Note("E"), Note("F")), (Note("G#"), Note("A")))
      .map {tuple => (tuple._1.get.halfStep.up, tuple._2.get)}
      .map {case (actualNote, expectedNote)
      => assert(actualNote == expectedNote)}

    List((Note("D"), Note("Db")), (Note("A"), Note("Ab")),
      (Note("Bb"), Note("A")), (Note("A#"), Note("A")),
      (Note("F"), Note("E")), (Note("Eb"), Note("D")))
      .map {tuple => (tuple._1.get.halfStep.down, tuple._2.get)}
      .map {case (actualNote, expectedNote)
      => assert(actualNote == expectedNote)}
  }
}

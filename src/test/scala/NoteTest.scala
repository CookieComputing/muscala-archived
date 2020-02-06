import org.scalatest.FunSuite

// Unit tests for the Note case class, as well as examples of how to use the class
class NoteTest extends FunSuite {
  test("Notes with letters in apply() should work") {
    assert(List("A", "B", "C", "D", "E", "F", "G")
      .map { Note(_) }
      .count(_.isDefined) == 7)
  }

  test("Notes with the same letter and octave should be the same") {
    List("A", "B", "C", "D", "E", "F", "G")
      .map { x => (Note(x), Note(x)) }
      .map { case (actualNote: Option[Note], expectedNote: Option[Note])
      => assert(actualNote == expectedNote) }
  }

  test ("Flats and sharps should cancel each other out") {
    List((Note.A.sharp.flat, Note.A),
      (Note.B.sharp.sharp.flat.flat, Note.B),
      (Note.C.flat.sharp.flat.sharp, Note.C),
      (Note.D.flat.flat.sharp.sharp, Note.D))
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote == expectedNote) }
  }

  test("Enharmonic notes should be the same") {
    // Since C is the start of the octave, we do not check B# and C (or vice versa) since they are on opposite edges
    // of the octave
    List((Note.C.sharp, Note.D.flat),
      (Note.D.sharp, Note.E.flat),
      (Note.E.sharp, Note.F),
      (Note.F.sharp, Note.G.flat),
      (Note.G.sharp, Note.A.flat),
      (Note.A.sharp, Note.B.flat))
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote == expectedNote)}
  }

  test("Multiple accidentals are allowed in Note apply() method") {
    assert(List(Note("C#"), Note("C##"), Note("C#b#b")).count(_.isDefined) == 3)
  }

  test("Using multiple accidentals in an apply should correctly adjust note") {
    List((Note("C##"), Note("D")),
      (Note("Dbb"), Note("C")),
      (Note("E##"), Note("F#")),
      (Note("F#b#b#b"), Note("F")))
      .map {tuple => (tuple._1.get, tuple._2.get)}
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote == expectedNote)}
  }
}

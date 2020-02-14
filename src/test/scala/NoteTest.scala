import note.Note
import org.scalatest.FunSuite

// Unit tests for the note.Note case class, as well as examples of how to use the class
class NoteTest extends FunSuite {
  test("Notes with letters in apply() should work") {
    assert(List("A", "B", "C", "D", "E", "F", "G")
      .map { Note(_) }
      .count(_.isDefined) == 7)
  }

  test("Notes with the same letter and octave should be the same") {
    List("A", "B", "C", "D", "E", "F", "G")
      .map { x => (Note(x).get, Note(x).get) }
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote == expectedNote)}
  }

  test ("Flats and sharps should cancel each other out") {
    List((Note.A.sharp.flat, Note.A),
      (Note.B.sharp.sharp.flat.flat, Note.B),
      (Note.C.flat.sharp.flat.sharp, Note.C),
      (Note.D.flat.flat.sharp.sharp, Note.D))
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote enharmonic expectedNote) }
  }

  test("A sharp/flat note should not equal the original note") {
    List((Note.A.sharp, Note.A),
      (Note.A.flat, Note.A),
      (Note.B.sharp, Note.B),
      (Note.B.flat, Note.B),
      (Note.C.sharp, Note.C),
      (Note.C.flat, Note.C),
      (Note.D.sharp, Note.D),
      (Note.D.flat, Note.D),
      (Note.E.sharp, Note.E),
      (Note.E.flat, Note.E),
      (Note.F.sharp, Note.F),
      (Note.F.flat, Note.F),
      (Note.G.sharp, Note.G),
      (Note.G.flat, Note.G)
    ).map { case (accidentalNote: Note, naturalNote: Note)
    => assert(accidentalNote != naturalNote) }
  }

  test("Enharmonic notes should be enharmonic, but not equal") {
    // Since C is the start of the octave, we do not check B# and C (or vice versa) since they are on opposite edges
    // of the octave
    List((Note.C.sharp, Note.D.flat),
      (Note.D.sharp, Note.E.flat),
      (Note.E.sharp, Note.F),
      (Note.F.sharp, Note.G.flat),
      (Note.G.sharp, Note.A.flat),
      (Note.A.sharp, Note.B.flat))
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote enharmonic expectedNote)}

    List((Note.C.sharp, Note.D.flat),
      (Note.D.sharp, Note.E.flat),
      (Note.F.sharp, Note.G.flat),
      (Note.G.sharp, Note.A.flat),
      (Note.A.sharp, Note.B.flat))
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote != expectedNote)}
  }

  test("Multiple accidentals are allowed in note.Note apply() method") {
    assert(List(Note("C#"), Note("C##"), Note("C#b#b")).count(_.isDefined) == 3)
  }

  test("Using multiple accidentals in an apply should correctly adjust note to enharmonic counterpart") {
    List((Note("C##"), Note("D")),
      (Note("Dbb"), Note("C")),
      (Note("E##"), Note("F#")),
      (Note("F#b#b#b"), Note("F")))
      .map {tuple => (tuple._1.get, tuple._2.get)}
      .map { case (actualNote: Note, expectedNote: Note)
      => assert(actualNote enharmonic expectedNote)}
  }

  test("Calling toString() should return the correct letters") {
    List((Note.A, "A"), (Note.B, "B"), (Note.C, "C"), (Note.D, "D"), (Note.E, "E"), (Note.F, "F"), (Note.G, "G"))
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toString == expectedLetter))}

    List((Note("A#"), "A#"), (Note("Ab"), "Ab"), (Note("Bb"), "Bb"), (Note("C#"), "C#"), (Note("Db"), "Db"),
      (Note("D#"), "D#"), (Note("Eb"), "Eb"), (Note("F#"), "F#"), (Note("Gb"), "Gb"), (Note("G#"), "G#"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toString == expectedLetter))}

    // Calling toString() will simply return the current state of the variables, without flatting them.
    List((Note("Cb"), "Cb"), (Note("B#"), "B#"), (Note("E#"), "E#"), (Note("Fb"), "Fb"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toString == expectedLetter))}

    // To convert them to "expected" notes, need to call backingNote()
    List((Note("Cb"), "B"), (Note("B#"), "C"), (Note("E#"), "F"), (Note("Fb"), "E"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.backingNote.toString == expectedLetter))}
  }

  test("Calling toStringWithOctave() should return the correct letters and octave") {
    List((Note("A", 1), "A-1"), (Note("B", 2), "B-2"), (Note("C", 3), "C-3"), (Note("D", 4), "D-4"),
      (Note("E", 5), "E-5"), (Note("F", 6), "F-6"), (Note("G", 7), "G-7"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toStringWithOctave == expectedLetter))}

    List((Note("A#", 1), "A#-1"), (Note("Ab", 1), "Ab-1"), (Note("Bb", 1), "Bb-1"), (Note("C#", 2), "C#-2"),
      (Note("Db", 2), "Db-2"), (Note("D#", 3), "D#-3"), (Note("Eb", 4), "Eb-4"), (Note("F#", 4), "F#-4"),
      (Note("Gb", 5), "Gb-5"), (Note("G#", 5), "G#-5"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toStringWithOctave == expectedLetter))}

    // Calling toStringWithOctave() will simply return the current state of the variables, without flatting them.
    // This is actually just going to recompute the value for edge cases. With B and C, they are on the edges of the
    // octave, so we adjust for the octave
    List((Note("Cb", 1001), "Cb-1001"), (Note("B#", 1336), "B#-1336"), (Note("E#", 42), "E#-42"), (Note("Fb", 8), "Fb-8"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toStringWithOctave == expectedLetter))}

    // To convert them to "expected" notes, need to call backingNote()
    List((Note("Cb", 1001), "B-1000"), (Note("B#", 1336), "C-1337"), (Note("E#", 42), "F-42"), (Note("Fb", 8), "E-8"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.backingNote.toStringWithOctave == expectedLetter))}
  }

  test("Attempting to flat the lowest note any further than possible will result in the same note") {
    val lowestNote = Note("C", 0).get
    assert(Note("C", 0).get.flat == lowestNote)
    assert(Note("Cbbbbbbbbb", 0).get.backingNote == lowestNote)
    assert(Note("Cbbbbbbbbb", -10).isEmpty)
  }

  test("Getting the backing note should work as expected") {
    List((Note("Ebbbb"), Note("C")),
      (Note("D####"), Note("F#")),
      (Note("Ebb##"), Note("E")),
      (Note("A#b#"), Note("A#")),
      (Note("Fb#b"), Note("E")),
      (Note("Gb#b"), Note("Gb")),
      (Note("C###b"), Note("D")))
      .map { tuple => (tuple._1.get, tuple._2.get)}
      .map {case (actualNote, expectedNote)
      => (assert (actualNote.backingNote == expectedNote))}
}

  test("Enharmonic notes have a distance of 0 half steps away") {
    List(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F)
      .map { note => (note, note) }
      .map { case (note1, note2)
      => assert(note1.distance(note2) == 0)}
  }

  test("Accidentals have a distance of one half step away") {
    // sharped notes are above the original note
    List(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F)
      .map { note => (note, note.sharp) }
      .map { case (note1, note2)
      => assert(note1.distance(note2) == 1)}

    // flatted notes are below the original note
    List(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F)
      .map { note => (note, note.flat) }
      .map { case (note1, note2)
      => assert(note1.distance(note2) == -1)}
  }

  test("Accidentals that have been cancelled should have the same distance as the original note") {
    List(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F)
      .map { note => (note, note.sharp.flat) }
      .map { case (note1, note2)
      => assert(note1.distance(note2) == 0)}

    List(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F)
      .map { note => (note, note.flat.sharp) }
      .map { case (note1, note2)
      => assert(note1.distance(note2) == 0)}

  }

  test("Octaves have a distance of 12 half steps away") {
    List(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F)
      .map { note => (note, Note(note.note, note.octave+1).get) }
      .map { case (note1, note2)
      => assert(note1.distance(note2) == 12)}
  }
}

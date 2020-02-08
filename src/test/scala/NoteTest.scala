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
      => assert(actualNote ==  expectedNote) }
  }

  test("An sharped/flat note should not equal the original note") {
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
    )
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

  test("Natural letter notes are natural") {
    val naturalNotes = List(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F, Note.G)
      naturalNotes
      .map {_.isNatural}
      .map {assert(_)}

    naturalNotes
      .map {_.isAccidental}
      .map {bool => assert(!bool)}
  }

  test("Accidental letter notes are accidental") {
    val accidentalNotes = List(Note("A#"), Note("Bb"), Note("C#"), Note("Db"), Note("D#"),
      Note("Eb"), Note("F#"), Note("Gb"), Note("G#"), Note("Ab"))

    accidentalNotes
      .map {_.get}
      .map {_.isAccidental}
      .map {assert(_)}

    accidentalNotes
      .map {_.get}
      .map {_.isNatural}
      .map {bool => assert(!bool)}
  }

  test("Natural notes raised or flatted can become accidental") {
    List(Note.A, Note.C, Note.D, Note.F, Note.G)
      .map {_.sharp}
      .map {_.isAccidental}
      .map {assert(_)}

    List(Note.A, Note.B, Note.D, Note.E, Note.G)
      .map {_.flat}
      .map {_.isAccidental}
      .map {assert(_)}

    List(Note.B, Note.E)
      .map {_.sharp}
      .map {_.isNatural}
      .map {assert(_)}

    List(Note.C, Note.F)
      .map {_.flat}
      .map {_.isNatural}
      .map {assert(_)}
  }

  test("Accidental notes raised or flatted can become natural") {
    List(Note("A#"), Note("C#"), Note("D#"), Note("F#"), Note("G#"))
      .map {_.get}
      .map {_.sharp}
      .map {_.isNatural}
      .map {assert(_)}

    List(Note("Ab"), Note("Bb"), Note("Db"), Note("Eb"), Note("Gb"))
      .map {_.get}
      .map {_.flat}
      .map {_.isNatural}
      .map {assert(_)}
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

    // This is actually just going to recompute the value for edge cases
    List((Note("Cb"), "B"), (Note("B#"), "C"), (Note("E#"), "F"), (Note("Fb"), "E"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toString == expectedLetter))}
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

    // This is actually just going to recompute the value for edge cases. With B and C, they are on the edges of the
    // octave, so we adjust for the octave
    List((Note("Cb", 1001), "B-1000"), (Note("B#", 1336), "C-1337"), (Note("E#", 42), "F-42"), (Note("Fb", 8), "E-8"))
      .map {tuple => (tuple._1.get, tuple._2)}
      .map {case (actualLetter, expectedLetter)
      => (assert (actualLetter.toStringWithOctave == expectedLetter))}
  }
}

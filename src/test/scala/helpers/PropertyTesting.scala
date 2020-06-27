package helpers

import chord.Chord
import note.Note
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

/**
  * Helper generators and properties for testing classes in the repository.
  */
object PropertyTesting {

  // Generates a random natural note of octave 4
  def naturalNoteGen: Gen[Note] = for {
    base <- Gen.oneOf(NoteTesting.naturalNoteSeq)
  } yield base

  // Generates a valid note string with possible sharps and/or flats
  def validNoteStringGen: Gen[String] = for {
    base <- Gen.oneOf(NoteTesting.naturalNoteSeq.map(_.note))
    numOfAccidentals <- Gen.choose(0, 20)
    accidentals <- Gen.listOfN(numOfAccidentals, Gen.oneOf(Note.Sharp, Note.Flat))
  } yield base + accidentals.foldLeft("")((acc, char) => acc + char)

  // Generates a random note of random octave, with possible sharps or flats.
  def noteGen: Gen[Note] = for {
    noteString <- validNoteStringGen
    octave <- arbitrary[Int]
  } yield Note(noteString, octave).get

  // Given a chord generating function, creates a generator that will generate arbitrary
  // chords to be used in property testing
  def chordGen[B <: Chord]: ((String => B) => Gen[B]) = createChord => for {
    base: String <- Gen.oneOf("A", "B", "C", "D", "E", "F", "G")
    numOfAccidentals <- Gen.choose(0, 20)
    flats <- Gen.listOfN(numOfAccidentals, Gen.const(Note.Flat))
    sharps <- Gen.listOfN(numOfAccidentals, Gen.const(Note.Sharp))
    accidentals <- Gen.option(Gen.oneOf(flats, sharps))
  } yield createChord(base + accidentals.getOrElse(List.empty[String]).foldLeft("")((acc, char) => acc + char))
}

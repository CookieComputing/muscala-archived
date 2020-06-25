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

  // Generates a random note of random octave, with possible sharps or flats.
  def noteGen: Gen[Note] = for {
    base <- Gen.oneOf(NoteTesting.naturalNoteSeq.map(_.note))
    octave <- arbitrary[Int]
    numOfAccidentals <- Gen.choose(0, 20)
    accidentals <- Gen.pick(numOfAccidentals, List(Note.Sharp, Note.Flat))
  } yield Note(base + accidentals.foldLeft("")((acc, char) => acc + char), octave).get

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

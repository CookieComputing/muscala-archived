package helpers

import chord.{Chord, SuspendedSecond}
import note.Note
import org.scalacheck.Gen

/**
  * Helper generators and properties for testing classes in the repository.
  */
object PropertyTesting {

  // Given a chord generating function, creates a generator that will generate arbitrary
  // chords to be used in property testing
  val chordGen: ((String => Chord) => Gen[Chord]) = createChord => for {
    base: String <- Gen.oneOf("A", "B", "C", "D", "E", "F", "G")
    numOfAccidentals <- Gen.choose(0, 20)
    flats <- Gen.listOfN(numOfAccidentals, Gen.const(Note.Flat))
    sharps <- Gen.listOfN(numOfAccidentals, Gen.const(Note.Sharp))
    accidentals <- Gen.option(Gen.oneOf(flats, sharps))
  } yield createChord(base + accidentals.getOrElse(List.empty[String]).foldLeft("")((acc, char) => acc + char))
}

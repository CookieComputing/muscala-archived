package helpers

import chord.Chord
import key.{Key, MajorKey, MinorKey}
import note.Note
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

/**
  * Helper generators and properties for testing classes in the repository.
  */
object PropertyTesting {

  /**
    * Generates a random natural note of octave 4
    * @return one of the natural notes
    */
  def naturalNoteGen: Gen[Note] = for {
    base <- Gen.oneOf(NoteTesting.naturalNoteSeq)
  } yield base

  /**
    * Generates a valid note string with possible sharps and/or flats
    * @return a valid string for a note
    */
  def validNoteStringGen: Gen[String] = for {
    base <- Gen.oneOf("A", "B", "C", "D", "E", "F", "G")
    numOfAccidentals <- Gen.choose(0, 20)
    accidentals <- Gen.listOfN(numOfAccidentals, Gen.oneOf(Note.Sharp, Note.Flat))
  } yield base + accidentals.foldLeft("")((acc, char) => acc + char)

  /**
    * Generates a random note of random octave, with possible sharps or flats.
    * @return a note
    */
  def noteGen: Gen[Note] = for {
    noteString <- validNoteStringGen
    octave <- arbitrary[Int]
  } yield Note(noteString, octave).get

  /**
    * Given a chord generating function, creates a generator that will
    * generate arbitrary chords to be used in property testing
    * @tparam B Any subtype of a chord
    * @return a valid chord
    */
  def chordGen[B <: Chord]: (String => B) => Gen[B] = createChord => for {
    validChordString <- validKeyOrChordStringGen
  } yield createChord(validChordString)

  /**
    * Generates either a valid major or minor key.
    * @return a valid key
    */
  def keyGen: Gen[Key] = for {
    validKeyString <- validKeyOrChordStringGen
    majorKey <- Gen.const(MajorKey(validKeyString).get)
    minorKey <- Gen.const(MinorKey(validKeyString).get)
    key <- Gen.oneOf(majorKey, minorKey)
  } yield key

  private def validKeyOrChordStringGen: Gen[String] = for {
    base: String <- Gen.oneOf("A", "B", "C", "D", "E", "F", "G")
    numOfAccidentals <- Gen.choose(0, 20)
    flats <- Gen.listOfN(numOfAccidentals, Gen.const(Note.Flat))
    sharps <- Gen.listOfN(numOfAccidentals, Gen.const(Note.Sharp))
    accidentals <- Gen.option(Gen.oneOf(flats, sharps))
  } yield base + accidentals.getOrElse(List.empty[String]).foldLeft("")((acc, char) => acc + char)
}

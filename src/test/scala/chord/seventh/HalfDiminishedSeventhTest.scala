package chord.seventh

import chord.Chord
import helpers.PropertyTesting
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for the half diminished seventh class
  */
class HalfDiminishedSeventhTest extends ASeventhTest {
  val chordGenerator: Gen[SeventhChord] = PropertyTesting.chordGen(HalfDiminishedSeventh(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C", "Eb", "G")),
    ("B", List("B", "D", "F", "A")),
    ("C", List("C", "Eb", "Gb", "Bb")),
    ("D", List("D", "F", "Ab", "C")),
    ("E", List("E", "G", "Bb", "D")),
    ("F", List("F", "Ab", "Cb", "Eb")),
    ("G", List("G", "Bb", "Db", "F")),
    ("Ab", List("Ab", "Cb", "Ebb", "Gb")),
    ("Bb", List("Bb", "Db", "Fb", "Ab")),
    ("Cb", List("Cb", "Ebb", "Gbb", "Bbb")),
    ("Db", List("Db", "Fb", "Abb", "Cb")),
    ("Eb", List("Eb", "Gb", "Bbb", "Db")),
    ("Fb", List("Fb", "Abb", "Cbb", "Ebb")),
    ("Gb", List("Gb", "Bbb", "Dbb", "Fb")),
    ("A#", List("A#", "C#", "E", "G#")),
    ("B#", List("B#", "D#", "F#", "A#")),
    ("C#", List("C#", "E", "G", "B")),
    ("D#", List("D#", "F#", "A", "C#")),
    ("E#", List("E#", "G#", "B", "D#")),
    ("F#", List("F#", "A", "C", "E")),
    ("G#", List("G#", "B", "D", "F#")),
  )
  val expectedChordName: String => String = tonic => tonic + Chord.minor + Chord.seventh + Chord.flat + Chord.fifth
  val chordApplyFunction: String => Option[SeventhChord] = tonic => HalfDiminishedSeventh(tonic)
  val expectedTriad: String => Chord = HalfDiminishedSeventh(_).get

  val expectedRootIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.minor.third) &&
        root.distance(fifth) == root.distance(root.diminished.fifth) &&
        root.distance(seventh) == root.distance(root.minor.seventh)
  }

  val expectedConsecutiveThirdIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.minor.third) &&
        third.distance(fifth) == third.distance(third.minor.third) &&
        fifth.distance(seventh) == fifth.distance(fifth.major.third)
  }

}

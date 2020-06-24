package chord.seventh

import chord.Chord
import chord.triad.{DiminishedTriad, MinorTriad}
import helpers.PropertyTesting
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for the diminished seventh chord
  */
class DiminishedSeventhTest extends ASeventhTest {
  val chordGenerator: Gen[SeventhChord] = PropertyTesting.chordGen(DiminishedSeventh(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C", "Eb", "Gb")),
    ("B", List("B", "D", "F", "Ab")),
    ("C", List("C", "Eb", "Gb", "Bbb")),
    ("D", List("D", "F", "Ab", "Cb")),
    ("E", List("E", "G", "Bb", "Db")),
    ("F", List("F", "Ab", "Cb", "Ebb")),
    ("G", List("G", "Bb", "Db", "Fb")),
    ("Ab", List("Ab", "Cb", "Ebb", "Gbb")),
    ("Bb", List("Bb", "Db", "Fb", "Abb")),
    ("Cb", List("Cb", "Ebb", "Gbb", "Bbbb")),
    ("Db", List("Db", "Fb", "Abb", "Cbb")),
    ("Eb", List("Eb", "Gb", "Bbb", "Dbb")),
    ("Fb", List("Fb", "Abb", "Cbb", "Ebbb")),
    ("Gb", List("Gb", "Bbb", "Dbb", "Fbb")),
    ("A#", List("A#", "C#", "E", "G")),
    ("B#", List("B#", "D#", "F#", "A")),
    ("C#", List("C#", "E", "G", "Bb")),
    ("D#", List("D#", "F#", "A", "C")),
    ("E#", List("E#", "G#", "B", "D")),
    ("F#", List("F#", "A", "C", "Eb")),
    ("G#", List("G#", "B", "D", "F")),
  )
  val expectedChordName: String => String = tonic => tonic + Chord.diminished + Chord.seventh
  val chordApplyFunction: String => Option[SeventhChord] = tonic => DiminishedSeventh(tonic)
  val expectedTriad: String => Chord = DiminishedTriad(_).get

  val expectedRootIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.minor.third) &&
        root.distance(fifth) == root.distance(root.diminished.fifth) &&
        root.distance(seventh) == root.distance(root.diminished.seventh)
  }

  val expectedConsecutiveThirdIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.minor.third) &&
        third.distance(fifth) == third.distance(third.minor.third) &&
        fifth.distance(seventh) == fifth.distance(fifth.minor.third)
  }
}

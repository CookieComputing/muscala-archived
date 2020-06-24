package chord.seventh

import chord.Chord
import chord.triad.MajorTriad
import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for the dominant seventh class
  */
class DominantSeventhTest extends ASeventhTest {
  val chordGenerator: Gen[SeventhChord] = PropertyTesting.chordGen(DominantSeventh(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C#", "E", "G")),
    ("B", List("B", "D#", "F#", "A")),
    ("C", List("C", "E", "G", "Bb")),
    ("D", List("D", "F#", "A", "C")),
    ("E", List("E", "G#", "B", "D")),
    ("F", List("F", "A", "C", "Eb")),
    ("G", List("G", "B", "D", "F")),
    ("Ab", List("Ab", "C", "Eb", "Gb")),
    ("Bb", List("Bb", "D", "F", "Ab")),
    ("Cb", List("Cb", "Eb", "Gb", "Bbb")),
    ("Db", List("Db", "F", "Ab", "Cb")),
    ("Eb", List("Eb", "G", "Bb", "Db")),
    ("Fb", List("Fb", "Ab", "Cb", "Ebb")),
    ("Gb", List("Gb", "Bb", "Db", "Fb")),
    ("A#", List("A#", "C##", "E#", "G#")),
    ("B#", List("B#", "D##", "F##", "A#")),
    ("C#", List("C#", "E#", "G#", "B")),
    ("D#", List("D#", "F##", "A#", "C#")),
    ("E#", List("E#", "G##", "B#", "D#")),
    ("F#", List("F#", "A#", "C#", "E")),
    ("G#", List("G#", "B#", "D#", "F#")),
  )
  val expectedChordName: String => String = tonic => tonic + Chord.seventh
  val chordApplyFunction: String => Option[SeventhChord] = tonic => DominantSeventh(tonic)
  val expectedTriad: String => Chord = MajorTriad(_).get

  val expectedRootIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.major.third) &&
        root.distance(fifth) == root.distance(root.perfect.fifth) &&
        root.distance(seventh) == root.distance(root.minor.seventh)
  }

  val expectedConsecutiveThirdIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.major.third) &&
        third.distance(fifth) == third.distance(third.minor.third) &&
        fifth.distance(seventh) == fifth.distance(fifth.minor.third)
  }

}

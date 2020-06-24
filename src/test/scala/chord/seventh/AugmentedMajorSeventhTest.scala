package chord.seventh

import chord.Chord
import chord.triad.{AugmentedTriad, MajorTriad}
import helpers.PropertyTesting
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for the augmented major seventh class
  */
class AugmentedMajorSeventhTest extends ASeventhTest {
  val chordGenerator: Gen[SeventhChord] = PropertyTesting.chordGen(AugmentedMajorSeventh(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C#", "E#", "G#")),
    ("B", List("B", "D#", "F##", "A#")),
    ("C", List("C", "E", "G#", "B")),
    ("D", List("D", "F#", "A#", "C#")),
    ("E", List("E", "G#", "B#", "D#")),
    ("F", List("F", "A", "C#", "E")),
    ("G", List("G", "B", "D#", "F#")),
    ("Ab", List("Ab", "C", "E", "G")),
    ("Bb", List("Bb", "D", "F#", "A")),
    ("Cb", List("Cb", "Eb", "G", "Bb")),
    ("Db", List("Db", "F", "A", "C")),
    ("Eb", List("Eb", "G", "B", "D")),
    ("Fb", List("Fb", "Ab", "C", "Eb")),
    ("Gb", List("Gb", "Bb", "D", "F")),
    ("A#", List("A#", "C##", "E##", "G##")),
    ("B#", List("B#", "D##", "F###", "A##")),
    ("C#", List("C#", "E#", "G##", "B#")),
    ("D#", List("D#", "F##", "A##", "C##")),
    ("E#", List("E#", "G##", "B##", "D##")),
    ("F#", List("F#", "A#", "C##", "E#")),
    ("G#", List("G#", "B#", "D##", "F##")),
  )
  val expectedChordName: String => String = tonic => tonic + Chord.major + Chord.seventh + Chord.sharp + Chord.fifth
  val chordApplyFunction: String => Option[SeventhChord] = tonic => AugmentedMajorSeventh(tonic)
  val expectedTriad: String => Chord = AugmentedTriad(_).get

  val expectedRootIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.major.third) &&
        root.distance(fifth) == root.distance(root.augmented.fifth) &&
        root.distance(seventh) == root.distance(root.major.seventh)
  }

  val expectedConsecutiveThirdIntervals: SeventhChord => Boolean = {
    chord =>
      val root = chord.notes.head
      val third = chord.notes(1)
      val fifth = chord.notes(2)
      val seventh = chord.notes.last
      root.distance(third) == root.distance(root.major.third) &&
        third.distance(fifth) == third.distance(third.major.third) &&
        fifth.distance(seventh) == fifth.distance(fifth.minor.third)
  }

}

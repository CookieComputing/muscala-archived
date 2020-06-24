package chord.seventh

import chord.Chord
import chord.triad.MajorTriad
import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for the major seventh chord
  */
class MajorSeventhTest extends ASeventhTest {
  val chordGenerator: Gen[SeventhChord] = PropertyTesting.chordGen(MajorSeventh(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C#", "E", "G#")),
    ("B", List("B", "D#", "F#", "A#")),
    ("C", List("C", "E", "G", "B")),
    ("D", List("D", "F#", "A", "C#")),
    ("E", List("E", "G#", "B", "D#")),
    ("F", List("F", "A", "C", "E")),
    ("G", List("G", "B", "D", "F#")),
    ("Ab", List("Ab", "C", "Eb", "G")),
    ("Bb", List("Bb", "D", "F", "A")),
    ("Cb", List("Cb", "Eb", "Gb", "Bb")),
    ("Db", List("Db", "F", "Ab", "C")),
    ("Eb", List("Eb", "G", "Bb", "D")),
    ("Fb", List("Fb", "Ab", "Cb", "Eb")),
    ("Gb", List("Gb", "Bb", "Db", "F")),
    ("A#", List("A#", "C##", "E#", "G##")),
    ("B#", List("B#", "D##", "F##", "A##")),
    ("C#", List("C#", "E#", "G#", "B#")),
    ("D#", List("D#", "F##", "A#", "C##")),
    ("E#", List("E#", "G##", "B#", "D##")),
    ("F#", List("F#", "A#", "C#", "E#")),
    ("G#", List("G#", "B#", "D#", "F##")),
  )
  val expectedChordName: String => String = tonic => tonic + Chord.major + Chord.seventh
  val chordApplyFunction: String => Option[SeventhChord] = tonic => MajorSeventh(tonic)
  val expectedTriad: String => Chord = MajorTriad(_).get
  val expectedFirstToSeventhDistance: (Note => Int) = root => root.distance(root.major.seventh)
}

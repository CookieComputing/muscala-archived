package chord.seventh

import chord.Chord
import chord.triad.MinorTriad
import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for the minor seventh chord
  */
class MinorSeventhTest extends ASeventhTest {
  val chordGenerator: Gen[SeventhChord] = PropertyTesting.chordGen(MinorSeventh(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C", "E", "G")),
    ("B", List("B", "D", "F#", "A")),
    ("C", List("C", "Eb", "G", "Bb")),
    ("D", List("D", "F", "A", "C")),
    ("E", List("E", "G", "B", "D")),
    ("F", List("F", "Ab", "C", "Eb")),
    ("G", List("G", "Bb", "D", "F")),
    ("Ab", List("Ab", "Cb", "Eb", "Gb")),
    ("Bb", List("Bb", "Db", "F", "Ab")),
    ("Cb", List("Cb", "Ebb", "Gb", "Bbb")),
    ("Db", List("Db", "Fb", "Ab", "Cb")),
    ("Eb", List("Eb", "Gb", "Bb", "Db")),
    ("Fb", List("Fb", "Abb", "Cb", "Ebb")),
    ("Gb", List("Gb", "Bbb", "Db", "Fb")),
    ("A#", List("A#", "C#", "E#", "G#")),
    ("B#", List("B#", "D#", "F##", "A#")),
    ("C#", List("C#", "E", "G#", "B")),
    ("D#", List("D#", "F#", "A#", "C#")),
    ("E#", List("E#", "G#", "B#", "D#")),
    ("F#", List("F#", "A", "C#", "E")),
    ("G#", List("G#", "B", "D#", "F#")),
  )
  val expectedChordName: String => String = tonic => tonic + Chord.minor + Chord.seventh
  val chordApplyFunction: String => Option[SeventhChord] = tonic => MinorSeventh(tonic)
  val expectedTriad: String => Chord = MinorTriad(_).get
  val expectedFirstToSeventhDistance: (Note => Int) = root => root.distance(root.minor.seventh)
}

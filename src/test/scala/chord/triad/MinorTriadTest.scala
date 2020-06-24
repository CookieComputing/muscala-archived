package chord.triad

import chord.Chord
import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

class MinorTriadTest extends ATriadTest {
  val chordGenerator: Gen[Chord] = PropertyTesting.chordGen(MinorTriad(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C", "E")),
    ("B", List("B", "D", "F#")),
    ("C", List("C", "Eb", "G")),
    ("D", List("D", "F", "A")),
    ("E", List("E", "G", "B")),
    ("F", List("F", "Ab", "C")),
    ("G", List("G", "Bb", "D")),
    ("A#", List("A#", "C#", "E#")),
    ("B#", List("B#", "D#", "F##")),
    ("C#", List("C#", "E", "G#")),
    ("D#", List("D#", "F#", "A#")),
    ("E#", List("E#", "G#", "B#")),
    ("F#", List("F#", "A", "C#")),
    ("G#", List("G#", "B", "D#")),
    ("Ab", List("Ab", "Cb", "Eb")),
    ("Bb", List("Bb", "Db", "F")),
    ("Cb", List("Cb", "Ebb", "Gb")),
    ("Db", List("Db", "Fb", "Ab")),
    ("Eb", List("Eb", "Gb", "Bb")),
    ("Fb", List("Fb", "Abb", "Cb")),
    ("Gb", List("Gb", "Bbb", "Db"))
  )

  val expectedChordName: String => String = tonic => tonic + Chord.minor
  val chordApplyFunction: String => Option[Chord] = tonic => MinorTriad(tonic)
  val firstToSecondDistance: (Note => Int) = root => root.distance(root.minor.third)
  val secondToThirdDistance: (Note => Int) = second => second.distance(second.major.third)
  val firstToThirdDistance: (Note => Int) = root => root.distance(root.perfect.fifth)
}

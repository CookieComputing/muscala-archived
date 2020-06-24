package chord.triad

import chord.Chord
import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

class MajorTriadTest extends ATriadTest {
  val chordGenerator: Gen[Chord] = PropertyTesting.chordGen(MajorTriad(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C#", "E")),
    ("B", List("B", "D#", "F#")),
    ("C", List("C", "E", "G")),
    ("D", List("D", "F#", "A")),
    ("E", List("E", "G#", "B")),
    ("F", List("F", "A", "C")),
    ("G", List("G", "B", "D")),
    ("A#", List("A#", "C##", "E#")),
    ("B#", List("B#", "D##", "F##")),
    ("C#", List("C#", "E#", "G#")),
    ("D#", List("D#", "F##", "A#")),
    ("E#", List("E#", "G##", "B#")),
    ("F#", List("F#", "A#", "C#")),
    ("G#", List("G#", "B#", "D#")),
    ("Ab", List("Ab", "C", "Eb")),
    ("Bb", List("Bb", "D", "F")),
    ("Cb", List("Cb", "Eb", "Gb")),
    ("Db", List("Db", "F", "Ab")),
    ("Eb", List("Eb", "G", "Bb")),
    ("Fb", List("Fb", "Ab", "Cb")),
    ("Gb", List("Gb", "Bb", "Db"))
  )

  val expectedChordName: String => String = tonic => tonic
  val chordApplyFunction: String => Option[Chord] = tonic => MajorTriad(tonic)
  val firstToSecondDistance: (Note => Int) = root => root.distance(root.major.third)
  val secondToThirdDistance: (Note => Int) = second => second.distance(second.minor.third)
  val firstToThirdDistance: (Note => Int) = root => root.distance(root.perfect.fifth)
}

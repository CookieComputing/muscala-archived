package chord.triad

import chord.Chord
import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for suspended second
  */
class SuspendedSecondTest extends ATriadTest {
  val chordGenerator: Gen[Chord] = PropertyTesting.chordGen(SuspendedSecond(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "B", "E")),
    ("B", List("B", "C#", "F#")),
    ("C", List("C", "D", "G")),
    ("D", List("D", "E", "A")),
    ("E", List("E", "F#", "B")),
    ("F", List("F", "G", "C")),
    ("G", List("G", "A", "D")),
    ("A#", List("A#", "B#", "E#")),
    ("B#", List("B#", "C##", "F##")),
    ("C#", List("C#", "D#", "G#")),
    ("D#", List("D#", "E#", "A#")),
    ("E#", List("E#", "F##", "B#")),
    ("F#", List("F#", "G#", "C#")),
    ("G#", List("G#", "A#", "D#")),
    ("Ab", List("Ab", "Bb", "Eb")),
    ("Bb", List("Bb", "C", "F")),
    ("Cb", List("Cb", "Db", "Gb")),
    ("Db", List("Db", "Eb", "Ab")),
    ("Eb", List("Eb", "F", "Bb")),
    ("Fb", List("Fb", "Gb", "Cb")),
    ("Gb", List("Gb", "Ab", "Db"))
  )

  val expectedChordName: String => String = tonic => tonic + Chord.suspended + Chord.second
  val chordApplyFunction: String => Option[Chord] = tonic => SuspendedSecond(tonic)
  val firstToSecondDistance: (Note => Int) = root => root.distance(root.major.second)
  val secondToThirdDistance: (Note => Int) = second => second.distance(second.perfect.fourth)
  val firstToThirdDistance: (Note => Int) = root => root.distance(root.perfect.fifth)
}

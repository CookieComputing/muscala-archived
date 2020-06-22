package chord.triad

import chord.Chord
import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for suspended fourths
  */
class SuspendedFourthTest extends ATriadTest {
  val chordGenerator: Gen[Chord] = PropertyTesting.chordGen(SuspendedFourth(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "D", "E")),
    ("B", List("B", "E", "F#")),
    ("C", List("C", "F", "G")),
    ("D", List("D", "G", "A")),
    ("E", List("E", "A", "B")),
    ("F", List("F", "Bb", "C")),
    ("G", List("G", "C", "D")),
    ("A#", List("A#", "D#", "E#")),
    ("B#", List("B#", "E#", "F##")),
    ("C#", List("C#", "F#", "G#")),
    ("D#", List("D#", "G#", "A#")),
    ("E#", List("E#", "A#", "B#")),
    ("F#", List("F#", "B", "C#")),
    ("G#", List("G#", "C#", "D#")),
    ("Ab", List("Ab", "Db", "Eb")),
    ("Bb", List("Bb", "Eb", "F")),
    ("Cb", List("Cb", "Fb", "Gb")),
    ("Db", List("Db", "Gb", "Ab")),
    ("Eb", List("Eb", "Ab", "Bb")),
    ("Fb", List("Fb", "Bbb", "Cb")),
    ("Gb", List("Gb", "Cb", "Db"))
  )

  val expectedChordName: String => String = tonic => tonic + Chord.suspended + Chord.fourth
  val chordApplyFunction: String => Option[Chord] = tonic => SuspendedFourth(tonic)
  val firstToSecondDistance: (Note => Int) = root => root.distance(root.perfect.fourth)
  val secondToThirdDistance: (Note => Int) = second => second.distance(second.major.second)
  val firstToThirdDistance: (Note => Int) = root => root.distance(root.perfect.fifth)
}

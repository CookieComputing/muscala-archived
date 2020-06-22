package chord

import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.TableFor2

/**
  * Unit tests for AugmentedTriad
  */
class AugmentedTriadTest extends ATriadTest {
  val chordGenerator: Gen[Chord] = PropertyTesting.chordGen(AugmentedTriad(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C#", "E#")),
    ("B", List("B", "D#", "F##")),
    ("C", List("C", "E", "G#")),
    ("D", List("D", "F#", "A#")),
    ("E", List("E", "G#", "B#")),
    ("F", List("F", "A", "C#")),
    ("G", List("G", "B", "D#")),
    ("A#", List("A#", "C##", "E##")),
    ("B#", List("B#", "D##", "F###")),
    ("C#", List("C#", "E#", "G##")),
    ("D#", List("D#", "F##", "A##")),
    ("E#", List("E#", "G##", "B##")),
    ("F#", List("F#", "A#", "C##")),
    ("G#", List("G#", "B#", "D##")),
    ("Ab", List("Ab", "C", "E")),
    ("Bb", List("Bb", "D", "F#")),
    ("Cb", List("Cb", "Eb", "G")),
    ("Db", List("Db", "F", "A")),
    ("Eb", List("Eb", "G", "B")),
    ("Fb", List("Fb", "Ab", "C")),
    ("Gb", List("Gb", "Bb", "D"))
  )

  val expectedChordName: String => String = tonic => tonic + Chord.augmented
  val chordApplyFunction: String => Option[Chord] = tonic => AugmentedTriad(tonic)
  val firstToSecondDistance: (Note => Int) = root => root.distance(root.major.third)
  val secondToThirdDistance: (Note => Int) = second => second.distance(second.major.third)
  val firstToThirdDistance: (Note => Int) = root => root.distance(root.perfect.fifth.sharp)
}

package chord

import helpers.PropertyTesting
import note.Note
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2

/**
  * Unit tests for DiminishedTriad
  */
class DiminishedTriadTest extends ATriadTest {
  val chordGenerator: Gen[Chord] = PropertyTesting.chordGen(DiminishedTriad(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", List("A", "C", "Eb")),
    ("B", List("B", "D", "F")),
    ("C", List("C", "Eb", "Gb")),
    ("D", List("D", "F", "Ab")),
    ("E", List("E", "G", "Bb")),
    ("F", List("F", "Ab", "Cb")),
    ("G", List("G", "Bb", "Db")),
    ("A#", List("A#", "C#", "E")),
    ("B#", List("B#", "D#", "F#")),
    ("C#", List("C#", "E", "G")),
    ("D#", List("D#", "F#", "A")),
    ("E#", List("E#", "G#", "B")),
    ("F#", List("F#", "A", "C")),
    ("G#", List("G#", "B", "D")),
    ("Ab", List("Ab", "Cb", "Ebb")),
    ("Bb", List("Bb", "Db", "Fb")),
    ("Cb", List("Cb", "Ebb", "Gbb")),
    ("Db", List("Db", "Fb", "Abb")),
    ("Eb", List("Eb", "Gb", "Bbb")),
    ("Fb", List("Fb", "Abb", "Cbb")),
    ("Gb", List("Gb", "Bbb", "Dbb"))
  )

  val expectedChordName: String => String = tonic => tonic + Chord.diminished
  val chordApplyFunction: String => Option[Chord] = tonic => DiminishedTriad(tonic)
  val firstToSecondDistance: (Note => Int) = root => root.distance(root.minor.third)
  val secondToThirdDistance: (Note => Int) = second => second.distance(second.minor.third)
  val firstToThirdDistance: (Note => Int) = root => root.distance(root.diminished.fifth)
}

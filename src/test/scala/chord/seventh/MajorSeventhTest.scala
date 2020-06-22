package chord.seventh

import chord.Chord
import helpers.PropertyTesting
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.TableFor2
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Unit tests for the major seventh chord
  */
class MajorSeventhTest extends FunSuite with ScalaCheckPropertyChecks{
  val chordGenerator: Gen[Chord] = PropertyTesting.chordGen(MajorSeventh(_).get)
  val chordNotes: TableFor2[String, List[String]] = Table(
    ("tonic", "chord notes"),
    ("A", ("A", "C#", "E", "G#")),
    ("B", ("B", "D#", "F#", "A#")),
    ("C", ("C", "E", "G", "B")),
    ("D", ("D", "F#", "A", "C")),
    ("E", ("E", "G#", "B", "D#")),
    ("F", ("F", "A", "C", "E")),
    ("G", ("G", "B", "D", "F#")),
  )
}

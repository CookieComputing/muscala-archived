package chord.seventh

import helpers.PropertyTesting
import interval.qualifier.Diatonic
import key.{Key, MajorKey, MinorKey}
import note.Note
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Unit tests for the Diatonic Seventh
  */
class DiatonicSeventhTest extends FunSuite with ScalaCheckPropertyChecks {
  val assertKeyChordFunction: (Key => String, String => SeventhChord, String => SeventhChord) => Unit = (rootNote, majorKeyChord, minorKeyChord) => {
    forAll(PropertyTesting.keyGen) {
      key =>
        val root = rootNote(key)
        val chord = DiatonicSeventh(root)(key).get
        key match {
          case MajorKey(_, _) => assert(chord.notes == majorKeyChord(root).notes)
          case MinorKey(_, _) => assert(chord.notes == minorKeyChord(root).notes)
        }
    }
  }

  test("Regardless of whether it's a minor or major key, the generated chord should" +
    "have the expected notes from the key") {
    forAll(PropertyTesting.keyGen) {
      key => forAll(Gen.oneOf(key.notes)) {
        rootNote =>
          val root = Note(rootNote).get
          val third = Diatonic.third(root)(key).get
          val fifth = Diatonic.fifth(root)(key).get
          val seventh = Diatonic.seventh(root)(key).get
          val chord = DiatonicSeventh(rootNote)(key).get
          assert(chord.notes.map(_.note) == List(root, third, fifth, seventh).map(_.note))
      }
    }
  }
  test("First note should correlate to the expected chord") {
    assertKeyChordFunction(k => k.notes.head,
      MajorSeventh(_).get,
      MinorSeventh(_).get)
  }

  test("Second note should correlate to the expected chord") {
    assertKeyChordFunction(k => k.notes(1),
      MinorSeventh(_).get,
      HalfDiminishedSeventh(_).get)
  }

  test("Third note should correlate to the expected chord") {
    assertKeyChordFunction(k => k.notes(2),
      MinorSeventh(_).get,
      MajorSeventh(_).get)
  }

  test("Fourth note should correlate to the expected chord") {
    assertKeyChordFunction(k => k.notes(3),
      MajorSeventh(_).get,
      MinorSeventh(_).get)
  }

  test("Fifth note should correlate to the expected chord") {
    assertKeyChordFunction(k => k.notes(4),
      DominantSeventh(_).get,
      MinorSeventh(_).get)
  }

  test("Sixth note should correlate to the expected chord") {
    assertKeyChordFunction(k => k.notes(5),
      MinorSeventh(_).get,
      MajorSeventh(_).get)
  }

  test("Seventh note should correlate to the expected chord") {
    assertKeyChordFunction(k => k.notes.last,
      HalfDiminishedSeventh(_).get,
      DominantSeventh(_).get)
  }

  test("invalid chords return nothing") {
    val invalidChordGen: Gen[String] = arbitrary[String].filterNot(tonic => MajorSeventh(tonic).isDefined)
    forAll(PropertyTesting.keyGen) {
      key =>
        forAll(invalidChordGen) {
          tonic =>
            assert(DiatonicSeventh(tonic)(key).isEmpty)
        }
    }
  }
}

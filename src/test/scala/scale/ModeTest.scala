package scale

import helpers.PropertyTesting
import key.{MajorKey, MinorKey}
import note.Note
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.TableFor3
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Unit tests for modes.
  */
class ModeTest extends FunSuite with ScalaCheckPropertyChecks {
  val modeTable: TableFor3[MajorKey, MajorKey => Mode, List[String]] = Table(
    ("Original key", "mode", "mode note sequence"),
    (MajorKey.C.toMajor, key => Ionian(key), List("C", "D", "E", "F", "G", "A", "B")),
    (MajorKey.C.toMajor, key => Dorian(key), List("D", "E", "F", "G", "A", "B", "C")),
    (MajorKey.C.toMajor, key => Phrygian(key), List("E", "F", "G", "A", "B", "C", "D")),
    (MajorKey.C.toMajor, key => Lydian(key), List("F", "G", "A", "B", "C", "D", "E")),
    (MajorKey.C.toMajor, key => Mixolydian(key), List("G", "A", "B", "C", "D", "E", "F")),
    (MajorKey.C.toMajor, key => Aeolian(key), List("A", "B", "C", "D", "E", "F", "G")),
    (MajorKey.C.toMajor, key => Locrian(key), List("B", "C", "D", "E", "F", "G", "A")),
  )

  val majorKeyGen: Gen[MajorKey] = for {
    key <- PropertyTesting.keyGen
  } yield key.toMajor

  val modeFromKeyGen: MajorKey => Gen[Mode] = majorKey => for {
    mode <- Gen.oneOf(Ionian(_), Dorian(_), Phrygian(_), Lydian(_), Mixolydian(_), Aeolian(_), Locrian(_))
  } yield mode(majorKey)

  test("a mode should contain all the notes of the original scale") {
    forAll(majorKeyGen) {
      key =>
        val modeGen = modeFromKeyGen(key)
        forAll(modeGen) {
          mode =>
            assert(key.notes.toSet == mode.notes.toSet)
        }
    }
  }

  test("a mode should contain a relative ordering of the original scale") {
    forAll(majorKeyGen) {
      key =>
        val modeGen = modeFromKeyGen(key)
        forAll(modeGen) {
          mode =>
            // This is analogous to a popular solution for "Is s1 an anagram of s2?"
            assert((mode.notes ++ mode.notes).foldLeft("")((acc, char) => acc + char)
              .contains(key.notes.foldLeft("")((acc, char) => acc + char)))
        }
    }
  }

  test("A minor scale takes the form of an aeolian mode") {
    val minorKeyGen: Gen[MinorKey] = for {
      key <- PropertyTesting.keyGen
    } yield key.toMinor

    forAll(minorKeyGen) {
      minorKey =>
        val majorKey = minorKey.toMajor
        assert(Aeolian(majorKey).notes == minorKey.notes)
    }
  }

  test(" A mode should produce the expected ordering of notes") {
    forAll(modeTable) {
      (key, modalFunction, expectedNotes) =>
        assert(modalFunction(key).notes == expectedNotes)
    }
  }

  test("An Ionian mode is effectively the major scale") {
    forAll(majorKeyGen) {
      majorKey =>
        assert(Ionian(majorKey).notes == majorKey.notes)
    }
  }

  test("A Dorian mode is effectively a minor scale with a major sixth") {
    forAll(majorKeyGen) {
      majorKey =>
        val minorKey = MinorKey(majorKey.notes(1)).get
        val sixth = Note(minorKey.notes(5)).get.sharp.note
        assert(Dorian(majorKey).notes == minorKey.notes.take(5) ++ List(sixth, minorKey.notes.last))
    }
  }

  test("A Phrygian mode is effectively a minor scale with a lowered second") {
    forAll(majorKeyGen) {
      majorKey =>
        val minorKey = MinorKey(majorKey.notes(2)).get
        val second = Note(minorKey.notes(1)).get.flat.note
        assert(Phrygian(majorKey).notes == List(minorKey.notes.head, second) ++ minorKey.notes.drop(2))
    }
  }

  test("A Lydian mode is effectively a major scale with a raised fourth") {
    forAll(majorKeyGen) {
      majorKey =>
        val lydianScale = MajorKey(majorKey.notes(3)).get
        val fourth = Note(lydianScale.notes(3)).get.sharp.note
        assert(Lydian(majorKey).notes == lydianScale.notes.take(3) ++ List(fourth) ++ lydianScale.notes.drop(4))
    }
  }

  test("A Mixolydian mode is effectively a major scale with a lowered seventh") {
    forAll(majorKeyGen) {
      majorKey =>
        val mixolydian = MajorKey(majorKey.notes(4)).get
        val seventh = Note(mixolydian.notes.last).get.flat.note
        assert(Mixolydian(majorKey).notes == mixolydian.notes.dropRight(1) ++ List(seventh))
    }
  }

  test("An Aeolian mode is effectively a minor scale") {
    forAll(majorKeyGen) {
      majorKey =>
        val minorKey = MinorKey(majorKey.notes(5)).get
        assert(Aeolian(majorKey).notes == minorKey.notes)
        assert(Aeolian(majorKey).notes == majorKey.toMinor.notes)
    }
  }

  test("A Locrian mode is effectively a minor scale with a lowered second and fifth") {
    forAll(majorKeyGen) {
      majorKey =>
        val minorKey = MinorKey(majorKey.notes.last).get
        val second = Note(minorKey.notes(1)).get.flat.note
        val fifth = Note(minorKey.notes(4)).get.flat.note
        assert(Locrian(majorKey).notes == List(minorKey.notes.head, second) ++ minorKey.notes.slice(2, 4) ++ List(fifth) ++ minorKey.notes.takeRight(2))
    }
  }
}

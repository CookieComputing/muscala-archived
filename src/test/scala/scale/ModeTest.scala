package scale

import helpers.PropertyTesting
import key.{MajorKey, MinorKey}
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

  test("a mode should contain all the notes of the original key") {
    forAll(majorKeyGen) {
      key =>
        val modeGen = modeFromKeyGen(key)
        forAll(modeGen) {
          mode =>
            assert(key.notes.toSet == mode.notes.toSet)
        }
    }
  }

  test("a mode should contain a relative ordering of the original key") {
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

  test("A minor key takes the form of an aeolian mode") {
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
}

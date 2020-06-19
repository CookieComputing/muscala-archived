package chord

import key.{Key, MajorKey, MinorKey}
import org.scalatest.FunSuite

/**
  * Unit tests for DiatonicTriad
  */
class DiatonicTriadTest extends FunSuite {
  test("Diatonic triads should have the expected chord qualities") {
    List(("C", MajorTriad(_)),
      ("D", MinorTriad(_)),
      ("E", MinorTriad(_)),
      ("F", MajorTriad(_)),
      ("G", MajorTriad(_)),
      ("A", MinorTriad(_)),
      ("B", DiminishedTriad(_)))
      .map {
        case (actual, expected) =>
          assert(DiatonicTriad(actual)(MajorKey.C).get == expected(actual).get)
      }

    List(("A", MinorTriad(_)),
      ("B", DiminishedTriad(_)),
      ("C", MajorTriad(_)),
      ("D", MinorTriad(_)),
      ("E", MinorTriad(_)),
      ("F", MajorTriad(_)),
      ("G", MajorTriad(_)))
      .map {
        case (actual, expected) =>
          assert(DiatonicTriad(actual)(MinorKey.A).get == expected(actual).get)
      }
  }

  test("Chords that do not belong in the key return nothing") {
    List("C#", "Db", "D#", "Eb", "F#", "Gb", "G#", "Ab", "A#", "Bb")
      .map {
        n =>
          assert(DiatonicTriad(n)(MajorKey.C).isEmpty)
          assert(DiatonicTriad(n)(MinorKey.A).isEmpty)
      }
  }

  test("Invalid chord values return nothing") {
    List("b", "", "31", "A###b", "?", "Ab#")
      .map {
        n =>
          assert(DiatonicTriad(n)(MajorKey.C).isEmpty)
          assert(DiatonicTriad(n)(MinorKey.A).isEmpty)
      }

  }
}

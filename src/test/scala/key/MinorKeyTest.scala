package key

import org.scalatest.FunSuite

/**
 * Unit tests for MinorKey
 */
class MinorKeyTest extends FunSuite {
  test("Natural letter keys are valid") {
    List("A", "B", "C", "D", "E", "F", "G")
      .map { MinorKey(_) }
      .map { key =>
        assert(key.isDefined)
      }
  }

  test("Keys with only sharps or flats are valid") {
    List(
      "A#",
      "Bb",
      "B#",
      "Cb",
      "C#",
      "Db",
      "D#",
      "Eb",
      "E#",
      "Fb",
      "F#",
      "Gb",
      "G#",
      "Ab"
    ).map { MajorKey(_) }
      .map { key =>
        assert(key.isDefined)
      }

    // Double accidentals and other possible theoretical keys are acceptable!
    List(
      "A##",
      "Bbb",
      "B##",
      "Cbb",
      "C##",
      "Dbb",
      "D##",
      "Ebb",
      "E##",
      "Fbb",
      "F##",
      "Gbb",
      "G##",
      "Abb"
    ).map { MinorKey(_) }
      .map { key =>
        assert(key.isDefined)
      }
  }

  test("Natural letter keys return expected notes in key") {
    List(
      ("A", List("A", "B", "C", "D", "E", "F", "G")),
      ("B", List("B", "C#", "D", "E", "F#", "G", "A")),
      ("C", List("C", "D", "Eb", "F", "G", "Ab", "Bb")),
      ("D", List("D", "E", "F", "G", "A", "Bb", "C")),
      ("E", List("E", "F#", "G", "A", "B", "C", "D")),
      ("F", List("F", "G", "Ab", "Bb", "C", "Db", "Eb"))
    ).map {
      case (tonic, expectedNotes) =>
        assert(MinorKey(tonic).get.notes == expectedNotes)
    }
  }

  test("The entire circle of minor fifths is supported") {
    List(
      ("Ab", List("Ab", "Bb", "Cb", "Db", "Eb", "Fb", "Gb")),
      ("Eb", List("Eb", "F", "Gb", "Ab", "Bb", "Cb", "Db")),
      ("Bb", List("Bb", "C", "Db", "Eb", "F", "Gb", "Ab")),
      ("F", List("F", "G", "Ab", "Bb", "C", "Db", "Eb")),
      ("C", List("C", "D", "Eb", "F", "G", "Ab", "Bb")),
      ("G", List("G", "A", "Bb", "C", "D", "Eb", "F")),
      ("D", List("D", "E", "F", "G", "A", "Bb", "C")),
      ("A", List("A", "B", "C", "D", "E", "F", "G")),
      ("E", List("E", "F#", "G", "A", "B", "C", "D")),
      ("B", List("B", "C#", "D", "E", "F#", "G", "A")),
      ("F#", List("F#", "G#", "A", "B", "C#", "D", "E")),
      ("C#", List("C#", "D#", "E", "F#", "G#", "A", "B")),
      ("G#", List("G#", "A#", "B", "C#", "D#", "E", "F#")),
      ("D#", List("D#", "E#", "F#", "G#", "A#", "B", "C#")),
      ("A#", List("A#", "B#", "C#", "D#", "E#", "F#", "G#"))
    ).map {
      case (tonic, expectedSignature) =>
        assert(MinorKey(tonic).get.notes == expectedSignature)
    }
  }

  test("The entire circle of minor fifth signatures return expected accidentals") {
    List(
      ("Ab", List("Ab", "Bb", "Cb", "Db", "Eb", "Fb", "Gb")),
      ("Eb", List("Eb", "Gb", "Ab", "Bb", "Cb", "Db")),
      ("Bb", List("Bb", "Db", "Eb", "Gb", "Ab")),
      ("F", List("Ab", "Bb", "Db", "Eb")),
      ("C", List("Eb", "Ab", "Bb")),
      ("G", List("Bb", "Eb")),
      ("D", List("Bb")),
      ("A", List()),
      ("E", List("F#")),
      ("B", List("C#", "F#")),
      ("F#", List("F#", "G#", "C#")),
      ("C#", List("C#", "D#", "F#", "G#")),
      ("G#", List("G#", "A#", "C#", "D#", "F#")),
      ("D#", List("D#", "E#", "F#", "G#", "A#", "C#")),
      ("A#", List("A#", "B#", "C#", "D#", "E#", "F#", "G#"))
    ).map {
      case (tonic, expectedSignature) =>
        assert(MinorKey(tonic).get.signature == expectedSignature)
    }
  }

  test("Other variations on a key tonic are invalid") {
    List(
      "Ab#",
      "Bb#",
      "Bb#",
      "Cb#",
      "Cb#",
      "Db##b#",
      "D##bb#",
      "Ebbbbb#",
      "E######b#",
      "Fbbbbb#",
      "F##b#",
      "Gb##b",
      "G##bb",
      "Abbb#",
      "",
      "a",
      "Az"
    ).map { MinorKey(_) }
      .map { key =>
        assert(key.isEmpty)
      }
  }
}

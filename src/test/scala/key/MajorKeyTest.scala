package key

import org.scalatest.FunSuite

/**
  * Unit tests for the Key class
  */
class MajorKeyTest extends FunSuite {
  test("Natural letter keys are valid") {
    List("A", "B", "C", "D", "E", "F", "G")
      .map { MajorKey(_) }
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
    ).map { MajorKey(_) }
      .map { key =>
        assert(key.isDefined)
      }
  }

  test("Natural letter keys return expected notes in key") {
    List(
      ("A", List("A", "B", "C#", "D", "E", "F#", "G#")),
      ("B", List("B", "C#", "D#", "E", "F#", "G#", "A#")),
      ("C", List("C", "D", "E", "F", "G", "A", "B")),
      ("D", List("D", "E", "F#", "G", "A", "B", "C#")),
      ("E", List("E", "F#", "G#", "A", "B", "C#", "D#")),
      ("F", List("F", "G", "A", "Bb", "C", "D", "E"))
    ).map {
      case (tonic, expectedNotes) =>
        assert(MajorKey(tonic).get.notes == expectedNotes)
    }
  }

  test("The entire circle of fifths is supported") {
    List(
      ("Cb", List("Cb", "Db", "Eb", "Fb", "Gb", "Ab", "Bb")),
      ("Gb", List("Gb", "Ab", "Bb", "Cb", "Db", "Eb", "F")),
      ("Db", List("Db", "Eb", "F", "Gb", "Ab", "Bb", "C")),
      ("Ab", List("Ab", "Bb", "C", "Db", "Eb", "F", "G")),
      ("Eb", List("Eb", "F", "G", "Ab", "Bb", "C", "D")),
      ("Bb", List("Bb", "C", "D", "Eb", "F", "G", "A")),
      ("F", List("F", "G", "A", "Bb", "C", "D", "E")),
      ("C", List("C", "D", "E", "F", "G", "A", "B")),
      ("G", List("G", "A", "B", "C", "D", "E", "F#")),
      ("D", List("D", "E", "F#", "G", "A", "B", "C#")),
      ("A", List("A", "B", "C#", "D", "E", "F#", "G#")),
      ("E", List("E", "F#", "G#", "A", "B", "C#", "D#")),
      ("B", List("B", "C#", "D#", "E", "F#", "G#", "A#")),
      ("F#", List("F#", "G#", "A#", "B", "C#", "D#", "E#")),
      ("C#", List("C#", "D#", "E#", "F#", "G#", "A#", "B#"))
    ).map {
      case (tonic, expectedNotes) =>
        assert(MajorKey(tonic).get.notes == expectedNotes)
    }
  }

  test("The entire circle of fifth signatures return expected accidentals") {
    List(
      ("Cb", List("Cb", "Db", "Eb", "Fb", "Gb", "Ab", "Bb")),
      ("Gb", List("Gb", "Ab", "Bb", "Cb", "Db", "Eb")),
      ("Db", List("Db", "Eb", "Gb", "Ab", "Bb")),
      ("Ab", List("Ab", "Bb", "Db", "Eb")),
      ("Eb", List("Eb", "Ab", "Bb")),
      ("Bb", List("Bb", "Eb")),
      ("F", List("Bb")),
      ("C", List()),
      ("G", List("F#")),
      ("D", List("F#", "C#")),
      ("A", List("C#", "F#", "G#")),
      ("E", List("F#", "G#", "C#", "D#")),
      ("B", List("C#", "D#", "F#", "G#", "A#")),
      ("F#", List("F#", "G#", "A#", "C#", "D#", "E#")),
      ("C#", List("C#", "D#", "E#", "F#", "G#", "A#", "B#"))
    ).map {
      case (tonic, expectedSignature) =>
        assert(MajorKey(tonic).get.signature == expectedSignature)
    }
  }

  test("Basic (single accidental) theoretical keys are supported") {
    List(
      ("G#", List("G#", "A#", "B#", "C#", "D#", "E#", "F##")),
      ("D#", List("D#", "E#", "F##", "G#", "A#", "B#", "C##")),
      ("A#", List("A#", "B#", "C##", "D#", "E#", "F##", "G##")),
      ("E#", List("E#", "F##", "G##", "A#", "B#", "C##", "D##")),
      ("B#", List("B#", "C##", "D##", "E#", "F##", "G##", "A##")),
      ("Fb", List("Fb", "Gb", "Ab", "Bbb", "Cb", "Db", "Eb"))
    ).map {
      case (tonic, expectedNotes) =>
        assert(MajorKey(tonic).get.notes == expectedNotes)
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
    ).map { MajorKey(_) }
      .map { key =>
        assert(key.isEmpty)
      }
  }

  test("The entire circle of fifths returns itself when converting to major") {
    List(
      "Cb",
      "Gb",
      "Ab",
      "Eb",
      "Bb",
      "F",
      "C",
      "G",
      "D",
      "A",
      "E",
      "B",
      "F#",
      "C#"
    ).map { tonic => assert(MajorKey(tonic).get.toMajor == MajorKey(tonic).get)}
  }

  test("The entire circle of fifths have their relative minor of fifths") {
    List(
      ("Cb", "Ab"),
      ("Gb", "Eb"),
      ("Db", "Bb"),
      ("Ab", "F"),
      ("Eb", "C"),
      ("Bb", "G"),
      ("F", "D"),
      ("C", "A"),
      ("G", "E"),
      ("D", "B"),
      ("A", "F#"),
      ("E", "C#"),
      ("B", "G#"),
      ("F#", "D#"),
      ("C#", "A#")
    ).map { case (majorKey, minorKey) =>
      assert(MajorKey(majorKey).get.toMinor == MinorKey(minorKey).get)
    }
  }
}

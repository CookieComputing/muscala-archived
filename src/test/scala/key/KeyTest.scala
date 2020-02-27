package key

import org.scalatest.FunSuite

/**
 * Unit tests for the Key class
 */
class KeyTest extends FunSuite {
  test("Natural letter keys are valid") {
    List("A", "B", "C", "D", "E", "F", "G")
      .map { Key(_) }
      .map {key => assert(key.isDefined) }
  }

  test("Keys with only sharps or flats are valid") {
    List("A#", "Bb", "B#", "Cb", "C#","Db", "D#", "Eb", "E#", "Fb", "F#", "Gb", "G#", "Ab")
      .map { Key(_) }
      .map {key => assert(key.isDefined) }

    // Double accidentals and other possible theoretical keys are acceptable!
    List("A##", "Bbb", "B##", "Cbb", "C##","Dbb", "D##", "Ebb", "E##", "Fbb", "F##", "Gbb", "G##", "Abb")
      .map { Key(_) }
      .map {key => assert(key.isDefined) }
  }

  test("Natural letter keys return expected key signatures") {
    List(
      ("A", List("A", "B", "C#", "D", "E", "F#", "G#")),
      ("B", List("B", "C#", "D#", "E", "F#", "G#", "A#")),
      ("C", List("C", "D", "E", "F", "G", "A", "B")),
      ("D", List("D", "E", "F#", "G", "A", "B", "C#")),
      ("E", List("E", "F#", "G#", "A", "B", "C#", "D#")),
      ("F", List("F", "G", "A", "Bb", "C", "D", "E"))
    ).map {
      case (tonic, expectedSignature) =>
        assert(Key(tonic).get.signature == expectedSignature)
    }
  }

  test("The entire circle of fifths is supported") {
   List (
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
     ("C#", List("C#", "D#", "E#", "F#", "G#", "A#", "B#")),
    ).map {
      case (tonic, expectedSignature) =>
        assert(Key(tonic).get.signature == expectedSignature)
    }
  }

  test("Basic (single accidental) theoretical key signatures are supported") {
    List (
      ("G#", List("G#", "A#", "B#", "C#", "D#", "E#", "F##")),
      ("D#", List("D#", "E#", "F##", "G#", "A#", "B#", "C##")),
      ("A#", List("A#", "B#", "C##", "D#", "E#", "F##", "G##")),
      ("E#", List("E#", "F#", "Eb", "Fb", "Gb", "Ab", "Bb")),
      ("B#", List("B#", "C##", "D##", "E#", "F##", "G##", "A##")),
      ("Fb", List("Fb", "Gb", "Ab", "Bbb", "Cb", "Db", "Eb")),
    )
  }

  test("Other variations on a key tonic are invalid") {
    List("Ab#", "Bb#", "Bb#", "Cb#", "Cb#","Db##b#",
      "D##bb#", "Ebbbbb#", "E######b#", "Fbbbbb#",
      "F##b#", "Gb##b", "G##bb", "Abbb#", "", "a", "Az")
      .map { Key(_) }
      .map {key => assert(key.isEmpty) }
  }
}

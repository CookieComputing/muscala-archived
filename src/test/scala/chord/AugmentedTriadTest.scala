package chord

import org.scalatest.FunSuite

/**
  * Unit tests for AugmentedTriad
  */
class AugmentedTriadTest extends FunSuite {
  test("augmented triad should correctly return the expected notes") {
    List(
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
    ).map { case (x, y) => (AugmentedTriad(x).get, y)}
      .map { case (actual, expected) =>
        assert(actual.notes.map(_.note) == expected)
        val first = actual.notes.head
        val third = actual.notes(1)
        val fifth = actual.notes(2)
        assert(first.distance(third) == first.distance(first.major.third))
        assert(third.distance(fifth) == third.distance(third.major.third))
        assert(first.distance(fifth) == first.distance(first.perfect.fifth.sharp))
        assert(actual.toString == first.note + Chord.augmented)
      }
  }

  test("Invalid augmented triads should return None") {
    assert(AugmentedTriad("b").isEmpty)
    assert(AugmentedTriad("Q").isEmpty)
    assert(AugmentedTriad("").isEmpty)
    assert(AugmentedTriad("1").isEmpty)
    assert(AugmentedTriad("!").isEmpty)
    assert(AugmentedTriad("Ab#").isEmpty)
  }

}

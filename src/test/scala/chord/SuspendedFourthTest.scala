package chord

import org.scalatest.FunSuite

/**
  * Unit tests for suspended fourths
  */
class SuspendedFourthTest extends FunSuite {
  test("suspended fourth should correctly return the expected notes") {
    List(
      ("A", List("A", "D", "E")),
      ("B", List("B", "E", "F#")),
      ("C", List("C", "F", "G")),
      ("D", List("D", "G", "A")),
      ("E", List("E", "A", "B")),
      ("F", List("F", "Bb", "C")),
      ("G", List("G", "C", "D")),
      ("A#", List("A#", "D#", "E#")),
      ("B#", List("B#", "E#", "F##")),
      ("C#", List("C#", "F#", "G#")),
      ("D#", List("D#", "G#", "A#")),
      ("E#", List("E#", "A#", "B#")),
      ("F#", List("F#", "B", "C#")),
      ("G#", List("G#", "C#", "D#")),
      ("Ab", List("Ab", "Db", "Eb")),
      ("Bb", List("Bb", "Eb", "F")),
      ("Cb", List("Cb", "Fb", "Gb")),
      ("Db", List("Db", "Gb", "Ab")),
      ("Eb", List("Eb", "Ab", "Bb")),
      ("Fb", List("Fb", "Bbb", "Cb")),
      ("Gb", List("Gb", "Cb", "Db"))
    ).map { case (x, y) => (SuspendedFourth(x).get, y)}
      .map { case (actual, expected) =>
        assert(actual.notes.map(_.note) == expected)
        val first = actual.notes.head
        val third = actual.notes(1)
        val fifth = actual.notes(2)
        assert(first.distance(third) == first.distance(first.perfect.fourth))
        assert(third.distance(fifth) == third.distance(third.major.second))
        assert(first.distance(fifth) == first.distance(first.perfect.fifth))
        assert(actual.toString == first.note + Chord.suspended + Chord.fourth)
      }
  }

  test("Invalid suspended seconds should return None") {
    assert(SuspendedFourth("b").isEmpty)
    assert(SuspendedFourth("Q").isEmpty)
    assert(SuspendedFourth("").isEmpty)
    assert(SuspendedFourth("1").isEmpty)
    assert(SuspendedFourth("!").isEmpty)
    assert(SuspendedFourth("Ab#").isEmpty)
  }
}

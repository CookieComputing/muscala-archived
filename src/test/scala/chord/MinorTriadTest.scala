package chord

import org.scalatest.FunSuite

class MinorTriadTest extends FunSuite {
  test("minor triad should correctly return the expected notes") {
    List(
      ("A", List("A", "C", "E")),
      ("B", List("B", "D", "F#")),
      ("C", List("C", "Eb", "G")),
      ("D", List("D", "F", "A")),
      ("E", List("E", "G", "B")),
      ("F", List("F", "Ab", "C")),
      ("G", List("G", "Bb", "D")),
      ("A#", List("A#", "C#", "E#")),
      ("B#", List("B#", "D#", "F##")),
      ("C#", List("C#", "E", "G#")),
      ("D#", List("D#", "F#", "A#")),
      ("E#", List("E#", "G#", "B#")),
      ("F#", List("F#", "A", "C#")),
      ("G#", List("G#", "B", "D#")),
      ("Ab", List("Ab", "Cb", "Eb")),
      ("Bb", List("Bb", "Db", "F")),
      ("Cb", List("Cb", "Ebb", "Gb")),
      ("Db", List("Db", "Fb", "Ab")),
      ("Eb", List("Eb", "Gb", "Bb")),
      ("Fb", List("Fb", "Abb", "Cb")),
      ("Gb", List("Gb", "Bbb", "Db"))
    ).map { case (x, y) => (MinorTriad(x).get, y)}
      .map { case (actual, expected) =>
        assert(actual.notes.map(_.note) == expected)
        val first = actual.notes.head
        val third = actual.notes(1)
        val fifth = actual.notes(2)
        assert(first.distance(third) == first.distance(first.minor.third))
        assert(third.distance(fifth) == third.distance(third.major.third))
        assert(first.distance(fifth) == first.distance(first.perfect.fifth))
        assert(actual.toString == first.note + Chord.minor)
      }
  }

  test("Invalid Minor triads should return None") {
    assert(MinorTriad("b").isEmpty)
    assert(MinorTriad("Q").isEmpty)
    assert(MinorTriad("").isEmpty)
    assert(MinorTriad("1").isEmpty)
    assert(MinorTriad("!").isEmpty)
    assert(MinorTriad("Ab#").isEmpty)
  }
}

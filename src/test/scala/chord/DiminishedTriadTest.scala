package chord

import org.scalatest.FunSuite

/**
  * Unit tests for DiminishedTriad
  */
class DiminishedTriadTest extends FunSuite {
  test("diminished triad should correctly return the expected notes") {
    List(
      ("A", List("A", "C", "Eb")),
      ("B", List("B", "D", "F")),
      ("C", List("C", "Eb", "Gb")),
      ("D", List("D", "F", "Ab")),
      ("E", List("E", "G", "Bb")),
      ("F", List("F", "Ab", "Cb")),
      ("G", List("G", "Bb", "Db")),
      ("A#", List("A#", "C#", "E")),
      ("B#", List("B#", "D#", "F#")),
      ("C#", List("C#", "E", "G")),
      ("D#", List("D#", "F#", "A")),
      ("E#", List("E#", "G#", "B")),
      ("F#", List("F#", "A", "C")),
      ("G#", List("G#", "B", "D")),
      ("Ab", List("Ab", "Cb", "Ebb")),
      ("Bb", List("Bb", "Db", "Fb")),
      ("Cb", List("Cb", "Ebb", "Gbb")),
      ("Db", List("Db", "Fb", "Abb")),
      ("Eb", List("Eb", "Gb", "Bbb")),
      ("Fb", List("Fb", "Abb", "Cbb")),
      ("Gb", List("Gb", "Bbb", "Dbb"))
    ).map { case (x, y) => (DiminishedTriad(x).get, y)}
      .map { case (actual, expected) =>
        assert(actual.notes.map(_.note) == expected)
        val first = actual.notes.head
        val third = actual.notes(1)
        val fifth = actual.notes(2)
        assert(first.distance(third) == first.distance(first.minor.third))
        assert(third.distance(fifth) == third.distance(third.minor.third))
        assert(first.distance(fifth) == first.distance(first.perfect.fifth.flat))
        assert(actual.toString == first.note + Chord.diminished)
      }
  }

  test("Invalid Diminished triads should return None") {
    assert(DiminishedTriad("b").isEmpty)
    assert(DiminishedTriad("Q").isEmpty)
    assert(DiminishedTriad("").isEmpty)
    assert(DiminishedTriad("1").isEmpty)
    assert(DiminishedTriad("!").isEmpty)
    assert(DiminishedTriad("Ab#").isEmpty)
  }
}

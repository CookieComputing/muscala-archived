package chord

import org.scalatest.FunSuite

class MajorTriadTest extends FunSuite {
  test("major triad should correctly return the expected notes") {
    List(
      ("A", List("A", "C#", "E")),
      ("B", List("B", "D#", "F#")),
      ("C", List("C", "E", "G")),
      ("D", List("D", "F#", "A")),
      ("E", List("E", "G#", "B")),
      ("F", List("F", "A", "C")),
      ("G", List("G", "B", "D")),
      ("A#", List("A#", "C##", "E#")),
      ("B#", List("B#", "D##", "F##")),
      ("C#", List("C#", "E#", "G#")),
      ("D#", List("D#", "F##", "A#")),
      ("E#", List("E#", "G##", "B#")),
      ("F#", List("F#", "A#", "C#")),
      ("G#", List("G#", "B#", "D#")),
      ("Ab", List("Ab", "C", "Eb")),
      ("Bb", List("Bb", "D", "F")),
      ("Cb", List("Cb", "Eb", "Gb")),
      ("Db", List("Db", "F", "Ab")),
      ("Eb", List("Eb", "G", "Bb")),
      ("Fb", List("Fb", "Ab", "Cb")),
      ("Gb", List("Gb", "Bb", "Db"))
    ).map { case (x, y) => (MajorTriad(x).get, y)}
        .map { case (actual, expected) =>
          assert(actual.notes.map(_.note) == expected)
          val first = actual.notes.head
          val third = actual.notes(1)
          val fifth = actual.notes(2)
          assert(first.distance(third) == first.distance(first.major.third))
          assert(third.distance(fifth) == third.distance(third.minor.third))
          assert(first.distance(fifth) == first.distance(first.perfect.fifth))
        }
  }

  test("Invalid Major triads should return None") {
    assert(MajorTriad("b").isEmpty)
    assert(MajorTriad("Q").isEmpty)
    assert(MajorTriad("").isEmpty)
    assert(MajorTriad("1").isEmpty)
    assert(MajorTriad("!").isEmpty)
    assert(MajorTriad("Ab#").isEmpty)
  }
}

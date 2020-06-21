package chord

import helpers.PropertyTesting
import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest.{FunSuite, FunSuiteLike}
import org.scalatestplus.scalacheck.Checkers

/**
  * Unit tests for suspended second
  */
class SuspendedSecondTest extends Properties("SuspendedSecond") with FunSuiteLike {
  val suspendedSecondChordGen: Gen[Chord] = PropertyTesting.chordGen(SuspendedSecond(_).get)

  property("Chord pattern should be tonic => major second from root => perfect fifth from root") =
    forAll(suspendedSecondChordGen) {
      chord =>
        val first = chord.notes.head
        val second = chord.notes(1)
        val third = chord.notes(2)

        first.distance(second) == first.distance(first.major.second) &&
        second.distance(third) == third.distance(third.perfect.fourth) &&
        first.distance(third) == first.distance(first.perfect.fifth)
    }

  property("Chord should have the suspended second in it's toString() method") =
    forAll(suspendedSecondChordGen) {
      chord => chord.toString == chord.tonic + Chord.suspended + Chord.second
    }

  test("suspended second should correctly return the expected notes") {
    List(
      ("A", List("A", "B", "E")),
      ("B", List("B", "C#", "F#")),
      ("C", List("C", "D", "G")),
      ("D", List("D", "E", "A")),
      ("E", List("E", "F#", "B")),
      ("F", List("F", "G", "C")),
      ("G", List("G", "A", "D")),
      ("A#", List("A#", "B#", "E#")),
      ("B#", List("B#", "C##", "F##")),
      ("C#", List("C#", "D#", "G#")),
      ("D#", List("D#", "E#", "A#")),
      ("E#", List("E#", "F##", "B#")),
      ("F#", List("F#", "G#", "C#")),
      ("G#", List("G#", "A#", "D#")),
      ("Ab", List("Ab", "Bb", "Eb")),
      ("Bb", List("Bb", "C", "F")),
      ("Cb", List("Cb", "Db", "Gb")),
      ("Db", List("Db", "Eb", "Ab")),
      ("Eb", List("Eb", "F", "Bb")),
      ("Fb", List("Fb", "Gb", "Cb")),
      ("Gb", List("Gb", "Ab", "Db"))
    ).map { case (x, y) => (SuspendedSecond(x).get, y)}
      .map { case (actual, expected) =>
        assert(actual.notes.map(_.note) == expected)
        val first = actual.notes.head
        val third = actual.notes(1)
        val fifth = actual.notes(2)
        assert(first.distance(third) == first.distance(first.major.second))
        assert(third.distance(fifth) == third.distance(third.perfect.fourth))
        assert(first.distance(fifth) == first.distance(first.perfect.fifth))
        assert(actual.toString == first.note + Chord.suspended + Chord.second)
      }
  }

  test("Invalid suspended second should return None") {
    assert(SuspendedSecond("b").isEmpty)
    assert(SuspendedSecond("Q").isEmpty)
    assert(SuspendedSecond("").isEmpty)
    assert(SuspendedSecond("1").isEmpty)
    assert(SuspendedSecond("!").isEmpty)
    assert(SuspendedSecond("Ab#").isEmpty)
  }
}

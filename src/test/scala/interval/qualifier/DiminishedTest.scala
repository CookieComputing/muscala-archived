package interval.qualifier

import helpers.NoteTesting
import org.scalatest.FunSuite

case class DiminishedTest() extends FunSuite {
  test("diminished fourth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "Db"),
        ("B", "Eb"),
        ("C", "Fb"),
        ("D", "Gb"),
        ("E", "Ab"),
        ("F", "A"),
        ("G", "Cb")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.diminished.fourth.toString == expectedNote.toString)
      }
  }

  test("diminished fourth notes are a distance of 4 half steps away") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.diminished.fourth)
      }
      .map {
        case (note, diminishedFourth) =>
          assert(note.distance(diminishedFourth) == 4)
      }
  }

  test("diminished fifth natural note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("A", "Eb"),
        ("B", "F"),
        ("C", "Gb"),
        ("D", "Ab"),
        ("E", "Bb"),
        ("F", "Cb"),
        ("G", "Db")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.diminished.fifth.toString == expectedNote.toString)
      }
  }

  test("diminished fifth notes are a distance of 6 half steps away") {
    NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq
      .map { note =>
        (note, note.diminished.fifth)
      }
      .map {
        case (note, diminishedFifth) =>
          assert(note.distance(diminishedFifth) == 6)
      }
  }

}

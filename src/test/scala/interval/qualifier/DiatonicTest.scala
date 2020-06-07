package interval.qualifier

import helpers.NoteTesting
import key.{Key, MajorKey, MinorKey}
import note.Note
import org.scalatest.FunSuite

class DiatonicTest extends FunSuite {
  test("unison should return the same note") {
    def correctUnison(noteToKey: String => Key) =
      (NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq)
        .map(n => noteToKey(n.note))
        .map {k => (k.notes, k)}
        .map {case (list, key) =>
          list.map{n =>
            assert(Diatonic.unison(Note(n).get)(key).contains(Note(n).get))}}

    correctUnison(MajorKey(_).get)
    correctUnison(MinorKey(_).get)

    assert(Diatonic.unison(Note("C").get)(MajorKey.B).isEmpty)
 }


  test("second should return the diatonic second with respect to the note") {
    (NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq)
      .map(n => MajorKey(n.note).get)
      .map {k => (k.notes, k)}
      .map {case (notes, k) => (notes.map(Note(_).get), k)}
      .map {case (notes, k) => notes.foldLeft(0) {
        case (index, note) =>
          val expectedNote = Note(k.notes((index+1) % k.notes.length)).get
          val actualNote = Diatonic.second(note)(k)
          assert(actualNote.isDefined)
          assert(actualNote.get.note == expectedNote.note)
          assert(note.distance(actualNote.get) == note.distance(note.minor.second)
          || note.distance(actualNote.get) == note.distance(note.major.second))
          index+1
      }
      }
  }

  test("third should return the diatonic third with respect to the note") {
    (NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq)
      .map(n => MajorKey(n.note).get)
      .map {k => (k.notes, k)}
      .map {case (notes, k) => (notes.map(Note(_).get), k)}
      .map {case (notes, k) => notes.foldLeft(0) {
        case (index, note) =>
          val expectedNote = Note(k.notes((index+2) % k.notes.length)).get
          val actualNote = Diatonic.third(note)(k)
          assert(actualNote.isDefined)
          assert(actualNote.get.note == expectedNote.note)
          assert(note.distance(actualNote.get) == note.distance(note.minor.third)
            || note.distance(actualNote.get) == note.distance(note.major.third))
          index+1
      }
      }
  }

  test("fourth should return the diatonic fourth with respect to the note") {
  }

  test("fifth should return the diatonic fifth with respect to the note") {
  }

  test("sixth should return the diatonic sixth with respect to the note") {
  }

  test("seventh should return the diatonic seventh with respect to the note") {
  }
}


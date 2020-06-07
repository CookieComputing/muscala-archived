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
    intervalTest(MajorKey(_).get,
      1,
      (note, key) => Diatonic.second(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.major.second)
        || note.distance(actualNote) == note.distance(note.minor.second))

    intervalTest(MinorKey(_).get,
      1,
      (note, key) => Diatonic.second(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.major.second)
        || note.distance(actualNote) == note.distance(note.minor.second))
  }

  test("third should return the diatonic third with respect to the note") {
    intervalTest(MajorKey(_).get,
      2,
      (note, key) => Diatonic.third(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.major.third)
    || note.distance(actualNote) == note.distance(note.minor.third))

    intervalTest(MinorKey(_).get,
      2,
      (note, key) => Diatonic.third(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.major.third)
        || note.distance(actualNote) == note.distance(note.minor.third))
  }

  test("fourth should return the diatonic fourth with respect to the note") {
    intervalTest(MajorKey(_).get,
      3,
      (note, key) => Diatonic.fourth(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.perfect.fourth)
    || note.distance(actualNote) == note.distance(note.perfect.fourth.flat)
    || note.distance(actualNote) == note.distance(note.perfect.fourth.sharp))

    intervalTest(MinorKey(_).get,
      3,
      (note, key) => Diatonic.fourth(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.perfect.fourth)
        || note.distance(actualNote) == note.distance(note.perfect.fourth.flat)
        || note.distance(actualNote) == note.distance(note.perfect.fourth.sharp))
  }

  test("fifth should return the diatonic fifth with respect to the note") {
    intervalTest(MajorKey(_).get,
      4,
      (note, key) => Diatonic.fifth(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.perfect.fifth)
        || note.distance(actualNote) == note.distance(note.perfect.fifth.flat))

    intervalTest(MinorKey(_).get,
      4,
      (note, key) => Diatonic.fifth(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.perfect.fifth)
        || note.distance(actualNote) == note.distance(note.perfect.fifth.flat))
  }

  test("sixth should return the diatonic sixth with respect to the note") {
    intervalTest(MajorKey(_).get,
      5,
      (note, key) => Diatonic.sixth(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.minor.sixth)
        || note.distance(actualNote) == note.distance(note.major.sixth))

    intervalTest(MinorKey(_).get,
      5,
      (note, key) => Diatonic.sixth(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.minor.sixth)
        || note.distance(actualNote) == note.distance(note.major.sixth))
  }

  test("seventh should return the diatonic seventh with respect to the note") {
    intervalTest(MajorKey(_).get,
      6,
      (note, key) => Diatonic.seventh(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.minor.seventh)
        || note.distance(actualNote) == note.distance(note.major.seventh))

    intervalTest(MinorKey(_).get,
      6,
      (note, key) => Diatonic.seventh(note)(key),
      (note, actualNote) => note.distance(actualNote) == note.distance(note.minor.seventh)
        || note.distance(actualNote) == note.distance(note.major.seventh))
  }

  private def intervalTest(noteToKey: String => Key, interval: Int, diatonicMethod: (Note, Key) => Option[Note], distanceCheck: (Note, Note) => Boolean) = {
    (NoteTesting.naturalNoteSeq ++ NoteTesting.accidentalNoteSeq)
      .map(n => noteToKey(n.note))
      .map {k => (k.notes, k)}
      .map {case (notes, k) => (notes.map(Note(_).get), k)}
      .map {case (notes, k) => notes.foldLeft(0) {
        case (index, note) =>
          val expectedNote = Note(k.notes((index+interval) % k.notes.length)).get
          val actualNote = diatonicMethod(note, k)
          assert(actualNote.isDefined)
          assert(actualNote.get.note == expectedNote.note)
          assert(distanceCheck(note, actualNote.get))
          index+1
      }
      }
  }
}


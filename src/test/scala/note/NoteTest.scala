package note

import helpers.{NoteTesting, PropertyTesting}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

// Unit tests for the note.Note case class, as well as examples of how to use the class
class NoteTest extends FunSuite with ScalaCheckPropertyChecks {
  test("Notes with any combination of sharps or flats are valid") {
    forAll(PropertyTesting.validNoteStringGen) {
      note =>
        assert(Note(note).isDefined)
    }
  }

  test("Flats and sharps should cancel each other out") {
    val balancedAccidentNoteGen: Note => Gen[Note] = note => for {
      numberOfAccidentals <- Gen.chooseNum(1, 100)
      flats <- Gen.listOfN(numberOfAccidentals, Gen.const(Note.Flat))
      sharps <- Gen.listOfN(numberOfAccidentals, Gen.const(Note.Sharp))
    } yield {
      val accidents = Random.shuffle(flats ++ sharps)
      accidents.foldLeft(note){
        (acc, accident) => accident match {
          case Note.Flat => acc.flat
          case Note.Sharp => acc.sharp
        }
      }
    }

    forAll(PropertyTesting.noteGen) {
      note => forAll(balancedAccidentNoteGen(note)) {
          balancedNote =>
            assert(note enharmonic balancedNote)
        }
    }
  }

  test("A sharp/flat note should not equal the original note") {
    val offsetNoteGen: Note => Gen[Note] = note => for {
      numOfAccidents <- Gen.chooseNum(1, 100)
      sharps <- Gen.listOfN(numOfAccidents, Note.Sharp)
      flats <- Gen.listOfN(numOfAccidents, Note.Flat)
      accidents <- Gen.oneOf(sharps, flats)
    } yield {
      accidents.foldLeft(note) {
        (acc, char) => char match {
          case Note.Sharp => acc.sharp
          case Note.Flat => note.flat
        }
      }
    }

    forAll(PropertyTesting.noteGen) {
      note => forAll(offsetNoteGen(note)) {
        offsetNote =>
          assert(note != offsetNote)
      }
    }
  }

  test("Enharmonic notes should be enharmonic, but not equal") {
    // Since C is the start of the octave, we do not check B# and C (or vice versa) since they are on opposite edges
    // of the octave
    List(
      (Note.C.sharp, Note.D.flat),
      (Note.D.sharp, Note.E.flat),
      (Note.E.sharp, Note.F),
      (Note.F.sharp, Note.G.flat),
      (Note.G.sharp, Note.A.flat),
      (Note.A.sharp, Note.B.flat)
    ).map {
      case (actualNote: Note, expectedNote: Note) =>
        assert(actualNote enharmonic expectedNote)
    }

    List(
      (Note.C.sharp, Note.D.flat),
      (Note.D.sharp, Note.E.flat),
      (Note.F.sharp, Note.G.flat),
      (Note.G.sharp, Note.A.flat),
      (Note.A.sharp, Note.B.flat)
    ).map {
      case (actualNote: Note, expectedNote: Note) =>
        assert(actualNote != expectedNote)
    }
  }

  test("Multiple accidentals are allowed in note.Note apply() method") {
    forAll(PropertyTesting.validNoteStringGen) {
      note =>
        assert(Note(note).isDefined)
    }
  }

  test(
    "Using multiple accidentals in an apply should correctly adjust note to enharmonic counterpart"
  ) {
    NoteTesting
      .toNoteTupleSeq(
        ("C##", "D"),
        ("Dbb", "C"),
        ("E##", "F#"),
        ("F#b#b#b", "F")
      )
      .map {
        case (actualNote: Note, expectedNote: Note) =>
          assert(actualNote enharmonic expectedNote)
      }
  }

  test("Calling toString() should return the correct letters") {
    List(
      (Note.A, "A"),
      (Note.B, "B"),
      (Note.C, "C"),
      (Note.D, "D"),
      (Note.E, "E"),
      (Note.F, "F"),
      (Note.G, "G")
    ).map {
      case (actualLetter, expectedLetter) =>
        assert(actualLetter.toString == expectedLetter)
    }

    List(
      (Note("A#"), "A#"),
      (Note("Ab"), "Ab"),
      (Note("Bb"), "Bb"),
      (Note("C#"), "C#"),
      (Note("Db"), "Db"),
      (Note("D#"), "D#"),
      (Note("Eb"), "Eb"),
      (Note("F#"), "F#"),
      (Note("Gb"), "Gb"),
      (Note("G#"), "G#")
    ).map { tuple =>
        (tuple._1.get, tuple._2)
      }
      .map {
        case (actualLetter, expectedLetter) =>
          assert(actualLetter.toString == expectedLetter)
      }

    // Calling toString() will simply return the current state of the variables, without flatting them.
    List(
      (Note("Cb"), "Cb"),
      (Note("B#"), "B#"),
      (Note("E#"), "E#"),
      (Note("Fb"), "Fb")
    ).map { tuple =>
        (tuple._1.get, tuple._2)
      }
      .map {
        case (actualLetter, expectedLetter) =>
          assert(actualLetter.toString == expectedLetter)
      }

    // To convert them to "expected" notes, need to call backingNote()
    List(
      (Note("Cb"), "B"),
      (Note("B#"), "C"),
      (Note("E#"), "F"),
      (Note("Fb"), "E")
    ).map { tuple =>
        (tuple._1.get, tuple._2)
      }
      .map {
        case (actualLetter, expectedLetter) =>
          assert(actualLetter.applyAccidentals.toString == expectedLetter)
      }
  }

  test(
    "Calling toStringWithOctave() should return the correct letters and octave"
  ) {
    List(
      (Note("A", 1), "A-1"),
      (Note("B", 2), "B-2"),
      (Note("C", 3), "C-3"),
      (Note("D", 4), "D-4"),
      (Note("E", 5), "E-5"),
      (Note("F", 6), "F-6"),
      (Note("G", 7), "G-7")
    ).map { tuple =>
        (tuple._1.get, tuple._2)
      }
      .map {
        case (actualLetter, expectedLetter) =>
          assert(actualLetter.toStringWithOctave == expectedLetter)
      }

    List(
      (Note("A#", 1), "A#-1"),
      (Note("Ab", 1), "Ab-1"),
      (Note("Bb", 1), "Bb-1"),
      (Note("C#", 2), "C#-2"),
      (Note("Db", 2), "Db-2"),
      (Note("D#", 3), "D#-3"),
      (Note("Eb", 4), "Eb-4"),
      (Note("F#", 4), "F#-4"),
      (Note("Gb", 5), "Gb-5"),
      (Note("G#", 5), "G#-5")
    ).map { tuple =>
        (tuple._1.get, tuple._2)
      }
      .map {
        case (actualLetter, expectedLetter) =>
          assert(actualLetter.toStringWithOctave == expectedLetter)
      }
  }

  test("toStringWithOctave() works with both applyAccidentals() and naturally") {
    List(
      (Note("Cb", 1001), "Cb-1000"),
      (Note("B#", 1336), "B#-1337"),
      (Note("E#", 42), "E#-42"),
      (Note("Fb", 8), "Fb-8")
    ).map { tuple =>
        (tuple._1.get, tuple._2)
      }
      .map {
        case (actualLetter, expectedLetter) =>
          assert(actualLetter.toStringWithOctave == expectedLetter)
      }

    // Notes may be confusing if redundant accidentals are not applied. In this case, it's alot clearer as to which
    // octaves notes belong in when they have their accidentals applied.
    List(
      (Note("Cb", 1001), "B-1000"),
      (Note("B#", 1336), "C-1337"),
      (Note("E#", 42), "F-42"),
      (Note("Fb", 8), "E-8")
    ).map { tuple =>
        (tuple._1.get, tuple._2)
      }
      .map {
        case (actualLetter, expectedLetter) =>
          assert(
            actualLetter.applyAccidentals.toStringWithOctave == expectedLetter
          )
      }
  }

  test("Getting the backing note should work as expected") {
    NoteTesting
      .toNoteTupleSeq(
        ("Ebbbb", "C"),
        ("D####", "F#"),
        ("Ebb##", "E"),
        ("A#b#", "A#"),
        ("Fb#b", "E"),
        ("Gb#b", "Gb"),
        ("C###b", "D")
      )
      .map {
        case (actualNote, expectedNote) =>
          assert(actualNote.applyAccidentals == expectedNote)
      }
  }

  test("Enharmonic notes have a distance of 0 half steps away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.enharmonic(note))
        assert(note.distance(note) == 0)
    }
  }

  test("Accidentals have a distance of one half step away") {
    forAll(PropertyTesting.noteGen) {
      note =>
        assert(note.distance(note.sharp) == 1)
        assert(note.distance(note.flat) == -1)
    }
  }

  test("Sharping a B to a C should increase the octave by one") {
    val bNoteGen: Gen[Note] = for {
      bNote <- Gen.const("B")
      octave <- Gen.chooseNum(-100000, 100000)
    } yield Note(bNote, octave).get

    forAll(bNoteGen) {
      note =>
        assert(note.sharp.octave == note.octave + 1)
    }
  }

  test("Invalid note construction should return None") {
    val invalidNoteGen: Gen[String] =
      arbitrary[String].filterNot(s =>
        "[A-G][#|b]*".matches(s))

    forAll(invalidNoteGen) {
      invalidNote =>
        assert(Note(invalidNote).isEmpty)
    }
  }
}

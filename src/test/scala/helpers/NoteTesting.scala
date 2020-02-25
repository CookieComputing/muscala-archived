package helpers

import note.Note

/**
  * Helper suite to provide convenience features for testing notes
  */
object NoteTesting {

  /**
    * @param list a list of valid strings to be processed as notes
    * @return a list of notes
    */
  def toNoteSeq(list: String*): Seq[Note] = list.map { Note(_).get }

  /**
    * Converts a set of valid strings to notes
    * @param list a tuple of strings containing valid strings
    * @return a seq of notes generated from the strings
    */
  def toNoteTupleSeq(list: (String, String)*): Seq[(Note, Note)] =
    list.map { tuple =>
      (Note(tuple._1).get, Note(tuple._2).get)
    }

  /**
    *  A list of natural notes to test
    * @return natural notes to test
    */
  def naturalNoteSeq: Seq[Note] =
    Seq(Note.A, Note.B, Note.C, Note.D, Note.E, Note.F, Note.G)

  /**
    * A list of accidental notes to test
    * @return accidental notes to test
    */
  def accidentalNoteSeq: Seq[Note] =
    toNoteSeq("A#", "Bb", "C#", "Db", "D#", "Eb", "F#", "G#", "Ab")
}

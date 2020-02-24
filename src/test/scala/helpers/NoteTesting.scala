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
  def toNoteSeq(list: String *): Seq[Note] = list.map { Note(_).get }

  def toNoteTupleSeq(list: (String, String) *): Seq[(Note, Note)] =
    list.map { tuple => (Note(tuple._1).get, Note(tuple._2).get) }
}

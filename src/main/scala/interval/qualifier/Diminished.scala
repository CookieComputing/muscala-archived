package interval.qualifier

import note.Note

/**
  * An interval qualifier for absolute diminished intervals
  *
  * @param note the note to move up from
  */
case class Diminished(note: Note) {

  /**
    * Represents a diminished fourth interval.
    * @return the diminished fourth interval note
    */
  def fourth: Note = Perfect(note).fourth.flat

  /**
    * Represents a diminished fifth interval.
    * @return the diminished fifth interval note
    */
  def fifth: Note = Perfect(note).fifth.flat

  /**
    * Represents a diminished seventh interval.
    * @return the diminished seventh interval note
    */
  def seventh: Note = Minor(note).seventh.flat
}

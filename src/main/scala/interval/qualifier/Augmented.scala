package interval.qualifier

import note.Note

/**
  * An interval qualifier for absolute augmented intervals
  * @param note the note to move up from
  */
case class Augmented(note: Note) {

  /**
    * Represents an augmented fourth interval.
    * @return the augmented fourth interval note
    */
  def fourth: Note = Perfect(note).fourth.sharp

  /**
    * Represents an augmented fifth interval.
    * @return the augmented fifth interval note
    */
  def fifth: Note = Perfect(note).fifth.sharp
}

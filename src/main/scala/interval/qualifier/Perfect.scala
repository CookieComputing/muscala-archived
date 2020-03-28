package interval.qualifier

import interval.movement.absolute
import interval.movement.absolute.NHalfSteps
import note.Note

/**
  * An interval qualifier for minor intervals on a note.
  *
  * @param note the note to move up from
  */
case class Perfect(note: Note) {

  /**
    * Represents a perfect fourth interval.
    * @return the perfect fourth interval note
    */
  def fourth: Note = absolute.NHalfSteps(note, 5).up

  /**
    * Represents a perfect fifth interval.
    * @return the perfect fifth interval note
    */
  def fifth: Note = absolute.NHalfSteps(note, 7).up

  /**
    * Represents a perfect octave interval.
    * @return the perfect octave interval note
    */
  def octave: Note = absolute.NHalfSteps(note, 12).up
}

package interval.qualifier

import interval.movement.NHalfSteps
import note.Note

/**
 * An interval qualifer for minor intervals on a note.
 *
 * @param note the note to move up from
 */
case class Perfect(note: Note) {

  /**
   * Represents a perfect fourth interval.
   * @return the perfect fourth interval note
   */
  def fourth: Note = NHalfSteps(note, 5).up

  /**
   * Represents a perfect fifth interval.
   * @return the perfect fifth interval note
   */
  def fifth: Note = NHalfSteps(note, 7).up

  /**
   * Represents a perfect octave interval.
   * @return the perfect octave interval note
   */
  def octave: Note = NHalfSteps(note, 12).up
}

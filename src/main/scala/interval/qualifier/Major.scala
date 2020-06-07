package interval.qualifier

import interval.movement
import interval.movement.NHalfSteps
import note.Note

/**
  * An interval qualifier for absolute major intervals on a note.
  *
  * @param note the note to move up from
  */
case class Major(note: Note) {

  /**
    * Represents a major second interval.
    * @return the major second interval note
    */
  def second: Note = NHalfSteps(note, 2).up

  /**
    * Represents a major third interval.
    * @return the major third interval note
    */
  def third: Note = movement.NHalfSteps(note, 4).up

  /**
    * Represents a major sixth interval.
    * @return the major sixth interval note
    */
  def sixth: Note = movement.NHalfSteps(note, 9).up

  /**
    * Represents a major seventh interval.
    * @return the major seventh interval note
    */
  def seventh: Note = movement.NHalfSteps(note, 11).up
}

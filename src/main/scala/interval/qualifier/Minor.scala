package interval.qualifier

import interval.movement.{HalfStep, NHalfSteps}
import note.Note

/**
  * An interval qualifer for minor intervals on a note.
  * @param note the note to move up from
  */
case class Minor(note: Note) {

  /**
    * Represents a minor second interval.
    * @return the minor second interval note
    */
  def second: Note = HalfStep(note).up

  /**
    * Represents a minor third interval.
    * @return the minor third interval note
    */
  def third: Note = NHalfSteps(note, 3).up

  /**
    * Represents a minor sixth interval.
    * @return the minor sixth interval note
    */
  def sixth: Note = NHalfSteps(note, 8).up

  /**
    * Represents a minor seventh interval.
    * @return the minor seventh interval note
    */
  def seventh: Note = NHalfSteps(note, 10).up
}

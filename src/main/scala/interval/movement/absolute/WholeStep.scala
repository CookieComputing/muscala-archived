package interval.movement.absolute

import interval.movement.IntervalMovement
import note.Note

/**
  * Represents a whole step change.
  */
case class WholeStep(note: Note) extends IntervalMovement {
  private val wholeStepMovement = NHalfSteps(note, 2)

  /**
    * Raises a note by a whole step
    * @return a note raised by a whole step
    */
  def up: Note = NHalfSteps(note, 2).up

  /**
    * Lowers a note by a whole step
    * @return a note lowered by a whole step
    */
  def down: Note = NHalfSteps(note, 2).down
}

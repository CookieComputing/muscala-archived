package interval.movement.diatonic

import interval.movement.{IntervalMovement, absolute}
import key.Key
import note.Note

/**
  * Represents a whole step change.
  */
case class WholeStep(rootNote: Note)(implicit val key: Key)
    extends IntervalMovement {
  private val wholeStepMovement = NHalfSteps(rootNote, 2)

  /**
    * Raises a note by a whole step
    * @return a note raised by a whole step
    */
  def up: Note = wholeStepMovement.up

  /**
    * Lowers a note by a whole step
    * @return a note lowered by a whole step
    */
  def down: Note = wholeStepMovement.down
}

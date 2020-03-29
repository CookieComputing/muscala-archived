package interval.movement.diatonic

import key.Key
import note.Note
import interval.movement.IntervalMovement

/**
  * Represents a half step change.
  */
case class HalfStep(note: Note)(implicit val key: Key) extends IntervalMovement {
  private val halfStepMovement = NHalfSteps(note, 1)

  /**
    * Raises the note by a half step
    * @return a note raised by a half step
    */
  def up: Note = halfStepMovement.up

  /**
    * Lowers a note by a half step
    * @return a note lowered by a half step
    */
  def down: Note = halfStepMovement.down
}

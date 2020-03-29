package interval.movement.diatonic

import interval.movement.IntervalMovement
import key.Key
import note.Note

/**
  * Provides movements from an arbitrary N steps. You probably want to use the Whole Step or Half Step classes for the
  * basic movements.
  */
case class NHalfSteps(note: Note, interval: Int)(implicit val key: Key) extends IntervalMovement {
  /**
    * Raises a note by n half steps
    * @return a note raised by n half steps
    */
  def up: Note = {
    note
  }

  /**
    * Lowers a note by n half steps
    * @return a note lowered by n half steps
    */
  def down: Note = {
    note
  }
}

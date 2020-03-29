package interval.movement.absolute

import interval.movement.IntervalMovement
import note.Note

/**
  * Represents a half step change.
  */
case class HalfStep(note: Note) extends IntervalMovement {
  /**
    * Raises the note by a half step
    * @return a note raised by a half step
    */
  // The current logic in the NHalfStep class does not permit the ability to move up 1 interval as a sharp would
  def up: Note = note.sharp

  /**
    * Lowers the note by a half step
    * @return a note lowered by a half step
    */
  def down: Note = note.flat
}

package interval.movement

import note.Note

/**
  * Represents a half step change.
  */
case class HalfStep(note: Note) extends IntervalMovement {
  val interval = 1

  // The current logic in the interval movement class does not permit the ability to move up 1 interval as a sharp would
  override def up: Note = note.sharp

  override def down: Note = note.flat
}

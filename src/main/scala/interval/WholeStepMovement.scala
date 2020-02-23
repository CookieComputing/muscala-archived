package interval

import note.Note

/**
 * Represents a whole step change.
 */
class WholeStepMovement(rootNote: Note) extends IntervalMovement {
  val note: Note = rootNote
  val interval = 2
}

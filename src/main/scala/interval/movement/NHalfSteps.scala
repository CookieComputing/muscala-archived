package interval.movement

import note.Note

/**
  * Provides movements from an arbitrary N steps. You probably want to use the Whole Step or Half Step classes for the
  * basic movements.
  */
case class NHalfSteps(note: Note, interval: Int) extends IntervalMovement

package interval.movement

import note.Note

/**
  * An interval movement is a change from a note's pitch up some interval.
  */
trait IntervalMovement {

  /**
    * Moves a note up by an interval.
    * @return a note raised by an interval
    */
  def up: Note

  /**
    * Moves a note down by an interval.
    * @return a note lowered by an interval
    */
  def down: Note
}

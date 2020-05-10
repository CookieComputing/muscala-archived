package interval.movement.diatonic

import interval.movement.IntervalMovement
import key.Key
import note.Note

/**
  * Provides movements from an arbitrary N steps. You probably want to use the Whole Step or Half Step classes for the
  * basic movements.
  */
case class NHalfSteps(note: Note, n: Int)(implicit val key: Key)
    extends IntervalMovement {

  /**
    * Raises a note by n half steps
    * @return a note raised by n half steps
    */
  def up: Note = alterNote(_.sharp)

  /**
    * Lowers a note by n half steps
    * @return a note lowered by n half steps
    */
  def down: Note = alterNote(_.flat)

  private def alterNote(modifyNote: Note => Note) = {
    val changedNote = (1 to n).foldLeft(note) { (acc, _) =>
      modifyNote(acc)
    }

    // TODO: Implement the diatonic changes with respect to a key
    ???
  }
}

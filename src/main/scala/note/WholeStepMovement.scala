package note

/**
 * Represents a whole step movement up or down.
 * @param note The note to change by a whole step.
 */
case class WholeStepMovement(note: Note) extends IntervalMovement {

  /**
   * Raises the note by a whole step
   * @return the Note raised by the amount specified
   */
  def up: Note = note.sharp.sharp

  /**
   * Lowers the note by a whole step
   * @return the Note lowered by the amount specified
   */
  def down: Note = note.flat.flat
}

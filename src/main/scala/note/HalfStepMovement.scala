package note

/**
 * Represents a half step movement either up or down.
 * @param note The note to change by a half step
 */
case class HalfStepMovement(note: Note) extends IntervalMovement {

  /**
   * Raises the note by a half step
   * @return the Note raised by the amount specified
   */
  def up: Note = note.sharp

  /**
   * Lowers the note by a half step
   * @return the Note lowered by the amount specified
   */
  def down: Note = note.flat
}

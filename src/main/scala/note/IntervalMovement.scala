package note

/**
 * A movement is the change of a note's pitch from it's original position to a new position by moving the note through
 * a certain amount of half steps
 */
trait IntervalMovement {
  val note: Note

  /**
   * Raises the note by the amount specified for the movement
   * @return the Note raised by the amount specified
   */
  def up: Note

  /**
   * Lowers the note by the amount specified for the movement
   * @return the Note lowered by the amount specified
   */
  def down: Note
}

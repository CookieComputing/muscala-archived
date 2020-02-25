package interval.qualifier

import note.Note

/**
 * An IntervalQualifier is the ability to create intervals that can move a note to different pitches.
 */
trait IntervalQualifier {
  protected val movableNote: Note

  /**
   * Returns a Perfect interval qualifer for this note.
   * @return the perfect interval qualifer for this note
   */
  def perfect: Perfect = Perfect(movableNote)

  /**
   * Returns a Major interval qualifer for this note.
   * @return the major interval qualifer for this note
   */
  def major: Major = Major(movableNote)

  /**
   * Returns a Minor interval qualifier for this note.
   * @return the minor interval qualifier for this note
   */
  def minor: Minor = Minor(movableNote)
}

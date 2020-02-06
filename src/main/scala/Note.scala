/**
 * The cornerstone of the library. Notes can be used independently to represent a single music note,
 * or in conjunction with other notes to represent chords.
 *
 * @param noteRank A number representing the distance that this note is from "C-0" in terms of the number of half steps.
 *                 The note "C-0" is chosen to be the fundamental "lowest" note,
 *                 and as such lower notes are not supported.
 */
case class Note private(noteRank: Int) {

  /**
   * Generates a note that is raised by one half step.
   *
   * @return a note that is one half step higher than this note
   */
  def sharp: Note = {
    Note(noteRank + 1)
  }

  /**
   * Generates a note that is lowered by one half step.
   *
   * @return a note that is one half step lower than this note. If
   *         the note is "C-0", return it
   */
  def flat: Note = {
    if (noteRank == 0) this else Note(noteRank - 1)
  }

  /**
   * @return the letter representing the note. If the note is an accidental, it will return a flat if the note
   *         was recently flatted, otherwise the note will be sharp.
   */
  override def toString: String = {
    "foo"
  }

  /**
   * @return A string in the form of "[letter]-[octave]". If the note is an accidental,
   *         it will return a flat if the note was recently flatted, otherwise the note will be sharp.
   */
  def toStringWithOctave: String = {
    "foo"
  }
}

object Note {
  private val halfStepsInOctave = 12
  private val noteRegex = "^[A-G][#|b]+$".r

  /**
   * A constructor which returns a note in the 4th octave.
   *
   * @param note A letter from A to G, ignoring case
   * @return The note in the 4th octave
   */
  def apply(note: String): Option[Note] = {
    Note(note, 4)
  }

  /**
   * A constructor which returns a note with a specified octave. If the note is invalid or
   * the octave is negative, returns None
   *
   * @param note   A letter from A to G, ignoring case
   * @param octave A number greater than zero
   * @return The note with the given octave
   */
  def apply(note: String, octave: Int): Option[Note] = {
    for {
      noteRank <- noteToInt(note)
      octaveMultiplier <- octaveToInt(octave)
    } yield (Note(noteRank + halfStepsInOctave * octaveMultiplier))
  }

  // Convenience wrappers for natural notes
  def A: Note = Note("A").get

  def B: Note = Note("B").get

  def C: Note = Note("C").get

  def D: Note = Note("D").get

  def E: Note = Note("E").get

  def F: Note = Note("F").get

  def G: Note = Note("G").get

  // Returns the modulo 12 mapping of a note, where "C" is 0 and "B" is 10.
  private def noteToInt(note: String): Option[Int] = note match {
    case "C" => Some(0)
    case "D" => Some(2)
    case "E" => Some(4)
    case "F" => Some(5)
    case "G" => Some(7)
    case "A" => Some(9)
    case "B" => Some(11)
    // Drops the letter in the string
    case noteRegex() => note.drop(1).foldLeft(noteToInt(note.take(1))) {
      (acc: Option[Int], accidental: Char) =>
        (acc, accidental) match {
          case (Some(acc), '#') => Some(acc + 1)
          case (Some(acc), 'b') => Some(acc - 1)
        }
    }
    case _ => None
  }

  private def octaveToInt(octave: Int): Option[Int] = octave match {
    case x if x >= 0 => Some(octave)
    case _ => None
  }
}

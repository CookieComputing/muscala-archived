package note

/**
 * The cornerstone of the library. Notes can be used independently to represent a single music note,
 * or in conjunction with other notes to represent chords. When testing for equality, you may need to consider using
 * enharmonic() instead of using the equality checker since notes may differ in being recently sharped or flatted.
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
    // We "skip" the flatted note rank to jump to the next note
    if (isSharp)
      Note(noteRank + 2)
    else Note(noteRank + 1)
  }

  /**
   * Generates a note that is lowered by one half step.
   *
   * @return a note that is one half step lower than this note. If
   *         the note is "C-0", return it
   */
  def flat: Note = {
    // We "skip" the sharped note rank to jump to the next note
    if (noteRank == 0) Note(0)
    else if (isFlat) Note(noteRank - 2)
    else Note(noteRank - 1)
  }

  /**
   * Determine if this is an accidental note
   * @return a boolean indicating if the node is accidental
   */
  def isAccidental: Boolean = !isNatural

  /**
   * Determine if this is a natural note.
   * @return a boolean indicating if the node is natural
   */
  def isNatural: Boolean = Note.naturalSet.contains(noteRankConverted)
  // this is an unfortunate consequence of how the ranking system is currently implemented.
  // perhaps this can be refactored once an implementation for scales is in place?

  /**
   * Determine if this is a flat note.
   * @return a boolean indicating if the node is flat
   */
  def isFlat: Boolean = Note.flatSet.contains(noteRankConverted)

  /**
   * Determine if this is a sharp note.
   * @return a boolean indicating if the note is sharp
   */
  def isSharp: Boolean = Note.sharpSet.contains(noteRankConverted)

  /**
   * Determines if two notes are enharmonic. This should be used for testing half step equality as opposed to
   * using the equality operator, since "Ab" and "G#" may not necessarily be equivalent.
   * @param other the other note to compare
   * @return whether or not the two notes are enharmonic.
   */
  def enharmonic(other: Note): Boolean =
    if (isNatural) noteRank == other.noteRank
    else if (isSharp && other.isFlat) noteRank + 1 == other.noteRank
    else if (isFlat && other.isSharp) other.noteRank == noteRank
    else false

  /**
   * Creates a half step movement from this note
   * @return a half step movement with this note
   */
  def halfStep: HalfStepMovement = HalfStepMovement(this)

  /**
   * Creates a whole step movement from this note
   * @return a whole step movement with this note
   */
  def wholeStep: WholeStepMovement = WholeStepMovement(this)

  private def noteRankConverted = noteRank % Note.noteRanksInOctave

  private def noteRankOctave: Int = noteRank / Note.noteRanksInOctave

  /**
   * @return the letter representing the note. If the note is an accidental, it will return a flat if the note
   *         was recently flatted, otherwise the note will be sharp.
   */
  override def toString: String = {
    if (isNatural) Note.intToNoteMapping.getOrElse(noteRankConverted, "")
    else if (isFlat) s"${Note(noteRank).sharp}b"
    else s"${Note(noteRank).flat}#"
  }

  /**
   * @return A string in the form of "[letter]-[octave]". If the note is an accidental,
   *         it will return a flat if the note was recently flatted, otherwise the note will be sharp.
   */
  def toStringWithOctave: String = s"${this.toString}-$noteRankOctave"
}

object Note {
  private val noteRanksInOctave = 17
  private val naturalNoteRegex = "^[A-G]$".r
  private val accidentalNoteRegex = "^[A-G][#|b]+$".r

  // The note ranking starts from 0 to 12, with "C" as the starting note and increasing in half steps
  private val naturalNoteMapping = Map[String, Int](
    "C" -> 0,
    "D" -> 3,
    "E" -> 6,
    "F" -> 7,
    "G" -> 10,
    "A" -> 13,
    "B" -> 16
  )
  private val intToNoteMapping = naturalNoteMapping.map(_.swap)
  // Hacky values determined by the ordering of the set: Refer to the wiki for more details
  private val naturalSet = Set(0, 3, 6, 7, 10, 13, 16)
  private val sharpSet = Set(1, 4, 8, 11, 14)
  private val flatSet = Set(2, 5, 9, 12, 15)

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
    note match {
      case naturalNoteRegex() =>
        for {
          noteRank <- noteToInt(note)
          octaveMultiplier <- octaveToInt(octave)
        } yield Note(noteRank + Note.noteRanksInOctave * octaveMultiplier)
      // Drops the letter in the string
      case accidentalNoteRegex() => note.drop(1).foldLeft(Note(note.take(1), octave)) {
        (acc: Option[Note], accidental: Char) =>
          (acc, accidental) match {
            case (Some(acc), '#') => Some(acc.sharp)
            case (Some(acc), 'b') => Some(acc.flat)
            case _ => None
          }
      }
      case _ => None
    }
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
    case naturalNoteRegex() => naturalNoteMapping.get(note)
    // Drops the letter in the string
    case accidentalNoteRegex() => note.drop(1).foldLeft(noteToInt(note.take(1))) {
      (acc: Option[Int], accidental: Char) =>
        (acc, accidental) match {
          case (Some(acc), '#') => Some(acc + 1)
          case (Some(acc), 'b') => Some(acc - 1)
          case _ => None
        }
    }
    case _ => None
  }

  private def octaveToInt(octave: Int): Option[Int] = octave match {
    case x if x >= 0 => Some(octave)
    case _ => None
  }
}

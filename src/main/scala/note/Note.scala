package note

/**
 * The cornerstone of the library. Notes can be used independently to represent a single music note,
 * or in conjunction with other notes to represent chords. When testing for equality, you may need to consider using
 * enharmonic() instead of using the equality checker since notes may differ in being recently sharped or flatted.
 *
 * @param note the note in the form of a string.
 * @param octave the octave that a note is in. The lowest note possible is "C-0", and any notes below are discouraged.
 */
case class Note private(note: String, octave: Int) {

  /**
   * Generates a note that is raised by one half step.
   *
   * @return a note that is one half step higher than this note
   */
  def sharp: Note = Note(note + Note.Sharp, octave).get

  /**
   * Generates a note that is lowered by one half step.
   *
   * @return a note that is one half step lower than this note. If
   *         the note is "C-0", return it
   */
  def flat: Note =
    if (note == "C" && octave <= 0) Note("C", 0).get
    else Note(note + Note.Flat, octave).get

  /**
   * Determine if this is an accidental note
   * @return a boolean indicating if the node is accidental
   */
  def isAccidental: Boolean = !isNatural

  /**
   * Determine if this is a natural note.
   * @return a boolean indicating if the node is natural
   */
  def isNatural: Boolean = Note.naturalSet.contains(note)

  /**
   * Determine if this is a flat note.
   * @return a boolean indicating if the node is flat
   */
  def isFlat: Boolean = backingNote.note.last == Note.Flat

  /**
   * Determine if this is a sharp note.
   * @return a boolean indicating if the note is sharp
   */
  def isSharp: Boolean = backingNote.note.last == Note.Sharp

  /**
   * Determines if two notes are enharmonic. This should be used for testing half step equality as opposed to
   * using the equality operator, since "Ab" and "G#" may not necessarily be equivalent.
   * @param other the other note to compare
   * @return whether or not the two notes are enharmonic.
   */
  def enharmonic(other: Note): Boolean = backingNote.noteToInt == other.noteToInt

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

  /**
   * Returns a "reduced" note in the event that the note has been flatted or sharped: This basically converts a note
   * by applying the accidentals sequentially. If a note still has accidentals by the end of the process, the last
   * accidental will be considered the canonical accidental after the previous accidentals have been applied.
   * @return A Note where all redundant accidentals have been applied
   */
  def backingNote: Note = note match {
      case Note.NaturalNoteRegex() => Note(note, octave).get
      case Note.AccidentalNoteRegex() => adjustedNote
    }

  private def adjustedNote: Note = {
    val adjustedNoteRank = noteToInt
    val adjustedOctave = adjustedNoteRank / Note.HalfStepsInOctave

    val noteRankRemainder = adjustedNoteRank % Note.HalfStepsInOctave
    if (Note.intToNaturalNoteMapping.contains(noteRankRemainder)) {
      Note(Note.intToNaturalNoteMapping(noteRankRemainder), adjustedOctave).get
    }
    else note.last match {
      case Note.Sharp => Note(Note.intToNaturalNoteMapping(noteRankRemainder-1) + Note.Sharp, adjustedOctave).get
      case Note.Flat => Note(Note.intToNaturalNoteMapping(noteRankRemainder+1) + Note.Flat, adjustedOctave).get
    }
  }

  private def noteToInt: Int = {
    val nonAdjustedNoteRank = octave * Note.HalfStepsInOctave + Note.naturalNoteMapping(note.take(1))
    note.drop(1).foldLeft(nonAdjustedNoteRank) {
      (acc: Int, accidental: Char) => (acc, accidental) match {
        case (acc: Int, Note.Flat) if acc > 0 => acc - 1
        case (acc: Int, Note.Sharp) => acc + 1
        case _ => acc
      }
    }
  }

  /**
   * @return the letter representing the note. A note is represented "as-is" in terms of the changes applied to it,
   *         so in order to see the changes reflected in the note, one of the "backing" methods should be used to
   *         convert the note to a more traditional appearance.
   */
  override def toString: String = note

  /**
   * @return A string in the form of "[letter]-[octave]". A note is represented "as-is" in terms of the changes applied
   *         to it, so in order to see the changes reflected in the note, one of the "backing" methods should be used to
   *         convert the note to a more traditional appearance.
   */
  def toStringWithOctave: String = s"${this.toString}-$octave"
}

object Note {
  private val Flat = 'b'
  private val Sharp = '#'
  private val HalfStepsInOctave = 12
  private val NaturalNoteRegex = "^[A-G]$".r
  private val AccidentalNoteRegex = "^[A-G][#|b]+$".r

  // The note ranking starts from 0 to 12, with "C" as the starting note and increasing in half steps
  private val naturalNoteMapping = Map[String, Int](
    "C" -> 0,
    "D" -> 2,
    "E" -> 4,
    "F" -> 5,
    "G" -> 7,
    "A" -> 9,
    "B" -> 11
  )
  private val intToNaturalNoteMapping = naturalNoteMapping.map(_.swap)
  // Hacky values determined by the ordering of the set: Refer to the wiki for more details
  private val naturalSet = naturalNoteMapping.keys.toSet

  /**
   * A constructor which returns a note in the 4th octave.
   *
   * @param note A letter from A to G, ignoring case
   * @return The note in the 4th octave
   */
  def apply(note: String): Option[Note] = Note(note, 4)

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
      case NaturalNoteRegex() if octave >= 0 => Some(new Note(note, octave))
      case AccidentalNoteRegex() if octave >= 0 => Some(new Note(note, octave))
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
}

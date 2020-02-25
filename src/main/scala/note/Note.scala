package note

import interval.qualifier.IntervalQualifier
import interval.movement.NoteMovement

/**
  * The cornerstone of the library. Notes can be used independently to represent a single music note,
  * or in conjunction with other notes to represent chords. When testing for equality, you may need to consider using
  * enharmonic() instead of using the equality checker since notes may differ in being recently sharped or flatted.
  *
  * @param note the note in the form of a string.
  * @param rank the number of half steps away from "C-0". It is possible to reach "negative" octaves, although the
  *             practicality of such a note is questionable.
  */
case class Note private (note: String, rank: Int)
    extends NoteMovement
    with IntervalQualifier {
  override protected val movableNote: Note = this

  /**
    * Returns the octave of this note.
    *
    * @return the note's octave
    */
  def octave: Int = rank / Note.HalfStepsInOctave

  /**
    * Generates a note that is raised by one half step.
    *
    * @return a note that is one half step higher than this note
    */
  def sharp: Note =
    if (note.last == Note.Flat) new Note(note.dropRight(1), rank + 1)
    else new Note(note + Note.Sharp, rank + 1)

  /**
    * Generates a note that is lowered by one half step.
    *
    * @return a note that is one half step lower than this note.
    */
  def flat: Note =
    if (note.last == Note.Sharp) new Note(note.dropRight(1), rank - 1)
    else new Note(note + Note.Flat, rank - 1)

  /**
    * Determines if two notes are enharmonic. This should be used for testing half step equality as opposed to
    * using the equality operator, since "Ab" and "G#" may not necessarily be equivalent.
    *
    * @param other the other note to compare
    * @return whether or not the two notes are enharmonic.
    */
  def enharmonic(other: Note): Boolean = rank == other.rank

  /**
    * Applies accidentals from this note to the note and converts it to the most "basic" note it can find. If the note
    * remains accidental by the end of the application, leaves the rightmost accidental on the note.
    *
    * @return the note with all accidentals applied
    */
  def applyAccidentals: Note = {
    val offset = Note.accidentals(this).foldLeft(0) { (acc, char) =>
      char match {
        case Note.Sharp => acc + 1
        case Note.Flat  => acc - 1
      }
    }
    val baseLetter = Note.naturalNoteMapping(Note.letter(this).toString)
    val remainder = (baseLetter + offset) % Note.HalfStepsInOctave
    val position =
      if (remainder < 0) remainder + Note.HalfStepsInOctave else remainder
    if (Note.intToNaturalNote.contains(position))
      new Note(Note.intToNaturalNote(position), rank)
    else if (note.last == Note.Sharp)
      new Note(Note.intToNaturalNote(position - 1) + Note.Sharp, rank)
    else
      new Note(Note.intToNaturalNote(position + 1) + Note.Flat, rank)
  }

  /**
    * Returns the number of half steps that the other note is away from this note. If the distance is positive, then
    * the other note is distance half steps above this note, and vice versa for negatives.
    *
    * @param other the other note to compare
    * @return the number of half steps these notes are apart. Positive means the other note is above this note.
    */
  def distance(other: Note): Int = other.rank - this.rank

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
  // The number of half steps in an octave.
  val HalfStepsInOctave = 12
  // Flat constant
  val Flat = 'b'
  // Sharp constant
  val Sharp = '#'
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
  private val intToNaturalNote = naturalNoteMapping.map(_.swap)
  // Hacky values determined by the ordering of the set: Refer to the wiki for more details

  private def octaveToRank(letter: String, octave: Int) =
    octave * Note.HalfStepsInOctave + Note.naturalNoteMapping(letter)

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
    * @param octave the note's octave
    * @return The note with the given octave
    */
  def apply(note: String, octave: Int): Option[Note] = {
    note match {
      case NaturalNoteRegex() =>
        Some(new Note(note, octaveToRank(note.take(1), octave)))
      case AccidentalNoteRegex() =>
        note.drop(1).foldLeft(Note(note.take(1), octave)) { (acc, accidental) =>
          accidental match {
            case Note.Sharp => acc.map { _.sharp }
            case Note.Flat  => acc.map { _.flat }
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

  /**
    * Convenience method to extract the letter from a note
    *
    * @param note the note to take a letter from
    * @return the letter of the note
    */
  def letter(note: Note): Char = note.note.charAt(0)

  /**
    * Convenience method to extract the accidentals from a note
    *
    * @param note the note to take accidentals from
    * @return the accidentals of a note (possibly empty)
    */
  def accidentals(note: Note): String = note.note.drop(1)
}

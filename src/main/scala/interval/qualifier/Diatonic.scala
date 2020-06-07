package interval.qualifier

import key.Key
import note.Note

/**
  * Represents diatonic interval operations. Diatonics can only work if a provided note fits within a key. Knowing
  * the key gives us the added information of knowing how exactly to apply certain operations such as getting the
  * third of a key.
  */
object Diatonic {

  /**
    * Returns the unison of the note
    * @param note the note to compare off of
    * @param key the key to calculate with
    */
  def unison(note: Note)(implicit key: Key): Option[Note] = interval(note, 0)

  /**
    * Returns the diatonic second of the note
    * @param note the note to compare off of
    * @param key the key to calculate with
    */
  def second(note: Note)(implicit key: Key): Option[Note] = interval(note, 1)

  /**
    * Returns the diatonic third of the note
    * @param note the note to compare off of
    * @param key the key to calculate with
    */
  def third(note: Note)(implicit key: Key): Option[Note] = interval(note, 2)

  /**
    * Returns the diatonic fourth of the note
    * @param note the note to compare off of
    * @param key the key to calculate with
    */
  def fourth(note: Note)(implicit key: Key): Option[Note] = interval(note, 3)

  /**
    * Returns the diatonic fifth of the note
    * @param note the note to compare off of
    * @param key the key to calculate with
    */
  def fifth(note: Note)(implicit key: Key): Option[Note] = interval(note, 4)

  /**
    * Returns the diatonic sixth of the note
    * @param note the note to compare off of
    * @param key the key to calculate with
    */
  def sixth(note: Note)(implicit key: Key): Option[Note] = interval(note, 5)

  /**
    * Returns the diatonic seventh of the note
    * @param note the note to compare off of
    * @param key the key to calculate with
    */
  def seventh(note: Note)(implicit key: Key): Option[Note] = interval(note, 6)

  // Helper to calculate the correct note to generate
  private def interval(note: Note, n: Int)(implicit key: Key): Option[Note] =
    for {
      srcNote <- noteOption(note)
      srcIndex <- noteIndexOption(srcNote)
      destIndex <- Some((srcIndex+n) % key.notes.length)
      destChar <- Some(key.notes(destIndex))
      destNote <- Note(destChar)
    } yield incrementNote(srcNote, destNote).copy(note=key.notes(destIndex))

  // Helper to convert note to option if note in key
  private def noteOption(note: Note)(implicit key: Key): Option[Note] = Some(note).filter(key.contains)

  @scala.annotation.tailrec
  private def incrementNote(current: Note, destNote: Note): Note = if (adjustRank(current) == adjustRank(destNote)) current else incrementNote(current.sharp, destNote)

  @scala.annotation.tailrec
  private def noteIndexOption(note: Note, i: Int = 0)(implicit key: Key): Option[Int] =
    if (i >= key.notes.length) None
    else if (adjustRank(Note(key.notes(i)).get) == adjustRank(note)) Some(i)
    else noteIndexOption(note, i+1)

  private def adjustRank(note: Note): Int = note.rank % Note.HalfStepsInOctave
}

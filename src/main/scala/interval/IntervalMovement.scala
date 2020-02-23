package interval

import note.Note

/**
 * An interval movement is a change from a note's pitch up some interval.
 */
abstract class IntervalMovement {
  // The note to change
  val note: Note

  // The interval that the note should change by.
  val interval: Int

  /**
   * Moves the note up by the number of half steps specified by the interval
   * @return
   */
  def up: Note = changeNote(_.sharp, {(x, y) => x + y})

  /**
   * Moves the note down by the number of half steps specified by the interval
   * @return
   */
  def down: Note = changeNote(_.flat, {(x, y) => x - y})

  private def changeNote(modifyNote: Note => Note, modifyInterval: (Int, Int) => Int) = {
    val rootNoteChanged = (1 to interval).foldLeft(Note(Note.letter(note).toString, note.octave)) {
      (acc, _) => acc.map { modifyNote(_) }
    }.get.applyAccidentals

    Note.accidentals(note).foldLeft(rootNoteChanged) {
      (acc, accidental) => accidental match {
        case Note.Sharp => acc.sharp
        case Note.Flat => acc.flat
      }
    }.copy(rank = modifyInterval(note.rank, interval))
  }
}

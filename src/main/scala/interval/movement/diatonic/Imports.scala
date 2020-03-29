package interval.movement.diatonic

import key.Key
import note.Note

/**
  * A diatonic wrapper for absolute interval movements. If a key is provided when moving a note using intervals, an
  * diatonic interval may be useful to ensure that the correct notes are used.
  */
object Imports {
  implicit class NoteMovement(val note: Note) {
    /**
      * Returns a whole step movement for this note.
      * @param key the key context
      * @return the whole step movement for this note
      */
    def wholeStep(implicit key: Key): WholeStep = WholeStep(note)

    /**
      * Returns a half step movement for this note.
      * @param key the key context
      * @return the half step movement for this note.
      */
    def halfStep(implicit key: Key): HalfStep = HalfStep(note)

    /**
      * Returns a N half step movement for this note.
      * @param n the number of half steps to move up by
      * @param key the key context
      * @return the n half step movement for this note.
      */
    def nHalfSteps(n: Int)(implicit key: Key): NHalfSteps = NHalfSteps(note, n)
  }
}

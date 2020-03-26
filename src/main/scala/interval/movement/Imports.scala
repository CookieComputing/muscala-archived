package interval.movement

import note.Note

/**
  * A NoteMovement is the ability to create movements that can move a note to different pitches.
  */
object Imports {
  implicit class NoteMovement(val note: Note) {
    /**
      * Returns a whole step movement for this note.
      * @return the whole step movement for this note
      */
    def wholeStep: WholeStep = WholeStep(note)

    /**
      * Returns a half step movement for this note.
      * @return the half step movement for this note.
      */
    def halfStep: HalfStep = HalfStep(note)

    /**
      * Returns a N half step movement for this note.
      * @param n the number of half steps to move up by
      * @return the n half step movement for this note.
      */
    def nHalfSteps(n: Int): NHalfSteps = NHalfSteps(note, n)
  }
}

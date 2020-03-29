package interval.movement.absolute

import interval.movement.IntervalMovement
import note.Note

/**
  * Provides movements from an arbitrary N steps. You probably want to use the Whole Step or Half Step classes for the
  * basic movements.
  */
case class NHalfSteps(note: Note, n: Int) extends IntervalMovement {

  /**
    * Raises a note by n half steps
    * @return a note raised by n half steps
    */
  def up: Note =
    changeNote(_.sharp, { (x, y) =>
      x + y
    })

  /**
    * Lowers a note by n half steps
    * @return a note lowered by n half steps
    */
  def down: Note =
    changeNote(_.flat, { (x, y) =>
      x - y
    })

  private def changeNote(
      modifyNote: Note => Note,
      modifyInterval: (Int, Int) => Int
  ) = {
    val rootNoteChanged = (1 to n)
      .foldLeft(Note(Note.letter(note).toString, note.octave)) { (acc, _) =>
        acc.map { modifyNote(_) }
      }
      .get
      .applyAccidentals

    Note
      .accidentals(note)
      .foldLeft(rootNoteChanged) { (acc, accidental) =>
        accidental match {
          case Note.Sharp => acc.sharp
          case Note.Flat  => acc.flat
        }
      }
      .copy(rank = modifyInterval(note.rank, n))
  }
}

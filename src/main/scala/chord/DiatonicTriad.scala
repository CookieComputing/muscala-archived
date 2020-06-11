package chord

import key.Key
import note.Note

/**
  * Provides a convenient wrapper for generating a chord from a tonic given a specific key.
  */
object DiatonicTriad {
  //TODO: Need to implemented diminished triad before this can be implemented
  def apply(tonic: String, key: Key): Option[Chord] = ???

  private def sameNoteDifferentOctave(first: Note, second: Note) =
    first.rank % Note.HalfStepsInOctave == second.rank % Note.HalfStepsInOctave

  @scala.annotation.tailrec
  private def increment(src: Note, dst: Note): Note = {
    if (sameNoteDifferentOctave(src, dst)) src else increment(src.sharp, dst)
  }
}

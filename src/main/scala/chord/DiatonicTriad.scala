package chord

import key.Key
import note.Note

/**
  * Represents a diatonic chord in a given key. A diatonic triad will take a tonic and look into the key to see
  * what the respective third and fifth of the chord should be.
  */
case class DiatonicTriad private (tonic: String, key: Key) extends Chord {
  // TODO: Design wise, why are we even defining a "diatonic triad"?
  //  In reality, this is just a major or minor triad. We can remove this logic and
  //  replace it with just a major or minor triad instead.
  override val notes: List[Note] = {
    val rootIndex = key.notes.indexOf(tonic)
    val thirdIndex = (rootIndex + 2) % key.notes.length
    val fifthIndex = (thirdIndex + 2) % key.notes.length

    val rootNote = Note(key.notes(rootIndex)).get
    val thirdNote = Note(key.notes(thirdIndex)).get
    val fifthNote = Note(key.notes(fifthIndex)).get
    List(
      rootNote,
      DiatonicTriad.increment(rootNote, thirdNote).copy(key.notes(thirdIndex)),
      DiatonicTriad.increment(rootNote, fifthNote).copy(key.notes(fifthIndex))
    )
  }
}

object DiatonicTriad {
  def apply(tonic: String, key: Key): Option[DiatonicTriad] =
    if (key.notes.contains(tonic)) Some(new DiatonicTriad(tonic, key)) else None

  private def sameNoteDifferentOctave(first: Note, second: Note) =
    first.rank % Note.HalfStepsInOctave == second.rank % Note.HalfStepsInOctave

  @scala.annotation.tailrec
  private def increment(src: Note, dst: Note): Note = {
    if (sameNoteDifferentOctave(src, dst)) src else increment(src.sharp, dst)
  }
}

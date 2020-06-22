package chord.seventh

import chord.Chord
import chord.triad.MajorTriad
import interval.qualifier.Diatonic
import key.{Key, MajorKey}
import note.Note

/**
  * Represents a major seventh. A major seventh is a major triad with a major seventh interval.
  */
case class MajorSeventh private (tonic: String) extends SeventhChord {
  private val key: Key = MajorKey(tonic).get
  override val triad: Chord = MajorTriad(tonic).get
  override val notes: List[Note] = {
    val root = triad.notes.head
    triad.notes ++ List(Diatonic.seventh(root)(key))
  }

  /**
    * Returns the major seventh in string format. Since major triads do not have
    * the maj qualifier, we add it here.
    * @return the chord in string format
    */
  override def toString: String = triad.toString + Chord.major + Chord.seventh
}

object MajorSeventh {
  def apply(tonic: String): Option[MajorSeventh] =
    MajorKey(tonic).map(_ => new MajorSeventh(tonic))
}

package chord

import key.MinorKey
import note.Note

/**
  * Represents a diminished triad. A diminished chord is defined as a minor third, followed by a minor third.
  */
case class DiminishedTriad private (override val tonic: String)
    extends ATriad(
      tonic,
      MinorKey(_).get,
      (n, k) => n.minor.third.copy(note = k.notes(2)),
      (n, k) => n.perfect.fifth.flat.copy(note = Note(k.notes(4)).get.flat.note)
    ) {
  override def toString: String = tonic + Chord.diminished
}

object DiminishedTriad {
  def apply(tonic: String): Option[DiminishedTriad] =
    MinorKey(tonic)
      .map(_ => new DiminishedTriad(tonic))
}

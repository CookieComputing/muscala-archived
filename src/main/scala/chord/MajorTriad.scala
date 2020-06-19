package chord

import key.{Key, MajorKey}

/**
  * Represents a major triad. A major chord is defined as a major third, followed by a minor third.
  */
case class MajorTriad private (override val tonic: String)
    extends ATriad(
      tonic,
      MajorKey(_).get,
      (n, k) => n.major.third.copy(note = k.notes(2)),
      (n, k) => n.perfect.fifth.copy(note = k.notes(4))
    ) {
  override def toString: String = tonic + Chord.major
}

object MajorTriad {
  def apply(tonic: String): Option[MajorTriad] =
    MajorKey(tonic)
      .map(_ => new MajorTriad(tonic))
}

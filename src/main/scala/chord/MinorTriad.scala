package chord

import key.MinorKey

/**
  * Represents a minor triad. A minor triad is defined as starting with a minor triad, followed by a major triad.
  */
case class MinorTriad private (override val tonic: String)
    extends ATriad(
      tonic,
      MinorKey(_).get,
      (n, k) => n.minor.third.copy(note = k.notes(2)),
      (n, k) => n.perfect.fifth.copy(note = k.notes(4))
    ) {
  override def toString: String = tonic + Chord.minor
}

object MinorTriad {
  def apply(tonic: String): Option[MinorTriad] =
    MinorKey(tonic)
      .map(_ => new MinorTriad(tonic))
}

package chord.triad

import chord.Chord
import interval.qualifier.Diatonic
import key.MinorKey

/**
  * Represents a minor triad. A minor triad is defined as starting with a minor triad, followed by a major triad.
  */
case class MinorTriad private (override val tonic: String)
    extends ATriad(
      tonic,
      MinorKey(_).get,
      (n, k) => Diatonic.third(n)(k).get,
      (n, k) => Diatonic.fifth(n)(k).get
    ) {
  override def toString: String = tonic + Chord.minor
}

object MinorTriad {
  def apply(tonic: String): Option[MinorTriad] =
    MinorKey(tonic)
      .map(_ => new MinorTriad(tonic))
}

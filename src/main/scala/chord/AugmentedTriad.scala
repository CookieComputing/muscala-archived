package chord

import interval.qualifier.Diatonic
import key.MajorKey
import note.Note

/**
  * Represents an augmented triad. An augmented chord is defined as a major third, followed by a major third.
  */
case class AugmentedTriad private (override val tonic: String)
    extends ATriad(
      tonic,
      MajorKey(_).get,
      (n, k) => Diatonic.third(n)(k).get,
      (n, k) => Diatonic.fifth(n)(k).get.sharp
    ) {
  override def toString: String = tonic + Chord.augmented
}

object AugmentedTriad {
  def apply(tonic: String): Option[AugmentedTriad] =
    MajorKey(tonic)
      .map(_ => new AugmentedTriad(tonic))
}

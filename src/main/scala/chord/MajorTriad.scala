package chord

import interval.qualifier.Diatonic
import key.{Key, MajorKey}

/**
  * Represents a major triad. A major chord is defined as a major third, followed by a minor third.
  */
case class MajorTriad private (override val tonic: String)
    extends ATriad(
      tonic,
      MajorKey(_).get,
      (n, k) => Diatonic.third(n)(k).get,
      (n, k) => Diatonic.fifth(n)(k).get
    ) {
  // Note that major triads typically omit the major qualifier, with the major qualifier name
  // usually appearing in a more complex chord instead
  override def toString: String = tonic
}

object MajorTriad {
  def apply(tonic: String): Option[MajorTriad] =
    MajorKey(tonic)
      .map(_ => new MajorTriad(tonic))
}

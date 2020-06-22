package chord

import interval.qualifier.Diatonic
import key.MajorKey

/**
  * A suspended fourth is built from a root, a perfect fourth, and a perfect fifth.
  */
case class SuspendedFourth private (tonic: String)
    extends ATriad(
      tonic,
      MajorKey(_).get,
      (n, k) => Diatonic.fourth(n)(k).get,
      (n, k) => Diatonic.fifth(n)(k).get
    ) {
  override def toString: String = tonic + Chord.suspended + Chord.fourth
}

object SuspendedFourth {
  def apply(tonic: String): Option[SuspendedFourth] =
    MajorKey(tonic).map(_ => new SuspendedFourth(tonic))
}

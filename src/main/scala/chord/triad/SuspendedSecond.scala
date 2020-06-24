package chord.triad

import chord.Chord
import interval.qualifier.Diatonic
import key.MajorKey

/**
  * A suspended fourth is built from a root, a major second, and a perfect fifth.
  */
case class SuspendedSecond private (tonic: String)
    extends ATriad(
      tonic,
      MajorKey(_).get,
      (n, k) => Diatonic.second(n)(k).get,
      (n, k) => Diatonic.fifth(n)(k).get
    ) {
  override def toString: String = tonic + Chord.suspended + Chord.second
}

object SuspendedSecond {
  def apply(tonic: String): Option[SuspendedSecond] =
    MajorKey(tonic).map(_ => new SuspendedSecond(tonic))
}

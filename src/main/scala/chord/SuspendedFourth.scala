package chord

import key.MajorKey

/**
  * A suspended fourth is built from a root, a perfect fourth, and a perfect fifth.
  */
case class SuspendedFourth private (tonic: String)
    extends ATriad(
      tonic,
      MajorKey(_).get,
      (n, k) => n.perfect.fourth.copy(note = k.notes(3)),
      (n, k) => n.perfect.fifth.copy(note = k.notes(4))
    ) {
  override def toString: String = tonic + Chord.suspended + Chord.fourth
}

object SuspendedFourth {
  def apply(tonic: String): Option[SuspendedFourth] =
    MajorKey(tonic).map(_ => new SuspendedFourth(tonic))
}

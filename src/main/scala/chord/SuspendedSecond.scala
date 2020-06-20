package chord

import key.MajorKey

/**
  * A suspended fourth is built from a root, a major second, and a perfect fifth.
  */
case class SuspendedSecond private (tonic: String)
  extends ATriad(tonic,
    MajorKey(_).get,
    (n, k) => n.major.second.copy(note = k.notes(1)),
    (n, k) => n.perfect.fifth.copy(note = k.notes(4))
  ) {
  override def toString: String = tonic + Chord.suspended + Chord.second
}

object SuspendedSecond {
  def apply(tonic: String): Option[SuspendedSecond] =
    MajorKey(tonic).map(_ => new SuspendedSecond(tonic))
}

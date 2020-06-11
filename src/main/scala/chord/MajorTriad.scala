package chord

import key.{Key, MajorKey}

/**
  * Represents a major triad. A major chord is defined as a major third, followed by a minor third.
  */
case class MajorTriad private (override val tonic: String)
    extends ATriad(tonic, MajorKey(_).get, n => n.major.third) {
  override def toString: String = tonic + Chord.major
}

object MajorTriad {
  def apply(tonic: String): Option[MajorTriad] =
    MajorKey(tonic)
      .map(_ => new MajorTriad(tonic))
}

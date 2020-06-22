package chord.triad

import chord.Chord
import interval.qualifier.Diatonic
import key.MinorKey

/**
  * Represents a diminished triad. A diminished chord is defined as a minor third, followed by a minor third.
  */
case class DiminishedTriad private (override val tonic: String)
    extends ATriad(
      tonic,
      MinorKey(_).get,
      (n, k) => Diatonic.third(n)(k).get,
      (n, k) => Diatonic.fifth(n)(k).get.flat
    ) {
  override def toString: String = tonic + Chord.diminished
}

object DiminishedTriad {
  def apply(tonic: String): Option[DiminishedTriad] =
    MinorKey(tonic)
      .map(_ => new DiminishedTriad(tonic))
}

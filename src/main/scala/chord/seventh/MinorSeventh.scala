package chord.seventh

import chord.Chord
import chord.triad.MinorTriad
import interval.qualifier.Diatonic
import key.MinorKey

/**
  * Represents a minor seventh. A minor seventh is a minor triad with a minor seventh interval.
  */
case class MinorSeventh private (tonic: String)
    extends ASeventh(
      tonic,
      n => MinorTriad(n).get,
      n => MinorKey(n).get,
      (n, k) => Diatonic.seventh(n)(k).get
    ) {

  /**
    * Returns the minor seventh in string format.
    * @return the chord in string format
    */
  override def toString: String = triad.toString + Chord.minor + Chord.seventh
}

object MinorSeventh {
  def apply(tonic: String): Option[MinorSeventh] =
    MinorKey(tonic).map(_ => new MinorSeventh(tonic))
}

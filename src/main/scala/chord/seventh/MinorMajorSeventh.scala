package chord.seventh

import chord.Chord
import chord.triad.MinorTriad
import interval.qualifier.Diatonic
import key.MinorKey

/**
  * Represents a minor major seventh. A minor seventh is a minor triad with a major seventh interval.
  */
case class MinorMajorSeventh private (tonic: String)
  extends ASeventh(
    tonic,
    n => MinorTriad(n).get,
    n => MinorKey(n).get,
    (n, k) => Diatonic.seventh(n)(k).get.sharp
  ) {

  /**
    * Returns the minor major seventh in string format.
    * @return the chord in string format
    */
  override def toString: String = triad.toString + Chord.major + Chord.seventh
}

object MinorMajorSeventh {
  def apply(tonic: String): Option[MinorMajorSeventh] =
    MinorKey(tonic).map(_ => new MinorMajorSeventh(tonic))
}

package chord.seventh

import chord.Chord
import chord.triad.MajorTriad
import interval.qualifier.Diatonic
import key.MajorKey

/**
  * Represents a dominant seventh. A major seventh is a major triad with a minor seventh interval.
  */
case class DominantSeventh private (override val tonic: String)
    extends ASeventh(
      tonic,
      n => MajorTriad(n).get,
      n => MajorKey(n).get,
      (n, k) => Diatonic.seventh(n)(k).get.flat
    ) {

  /**
    * Returns the dominant seventh in string format.
    * @return the chord in string format
    */
  override def toString: String = triad.toString + Chord.seventh
}

object DominantSeventh {
  def apply(tonic: String): Option[DominantSeventh] =
    MajorKey(tonic).map(_ => new DominantSeventh(tonic))
}

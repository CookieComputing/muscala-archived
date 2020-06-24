package chord.seventh

import chord.Chord
import chord.triad.AugmentedTriad
import interval.qualifier.Diatonic
import key.MajorKey

/**
  * Represents an augmented major seventh. An augmented major seventh is an augmented triad with a major seventh interval.
  */
case class AugmentedMajorSeventh private (override val tonic: String)
    extends ASeventh(
      tonic,
      n => AugmentedTriad(n).get,
      n => MajorKey(n).get,
      (n, k) => Diatonic.seventh(n)(k).get
    ) {

  /**
    * Returns the augmented major seventh in string format.
    * We use the jazz terminology for the augmented major seventh.
    * @return the chord in string format
    */
  override def toString: String =
    tonic + Chord.major + Chord.seventh + Chord.sharp + Chord.fifth
}

object AugmentedMajorSeventh {
  def apply(tonic: String): Option[AugmentedMajorSeventh] =
    MajorKey(tonic).map(_ => new AugmentedMajorSeventh(tonic))
}

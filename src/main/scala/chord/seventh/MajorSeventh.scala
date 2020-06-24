package chord.seventh

import chord.Chord
import chord.triad.MajorTriad
import interval.qualifier.Diatonic
import key.MajorKey

/**
  * Represents a major seventh. A major seventh is a major triad with a major seventh interval.
  */
case class MajorSeventh private (override val tonic: String)
    extends ASeventh(
      tonic,
      n => MajorTriad(n).get,
      n => MajorKey(n).get,
      (n, k) => Diatonic.seventh(n)(k).get
    ) {

  /**
    * Returns the major seventh in string format. Since major triads do not have
    * the maj qualifier, we add it here.
    * @return the chord in string format
    */
  override def toString: String = triad.toString + Chord.major + Chord.seventh
}

object MajorSeventh {
  def apply(tonic: String): Option[MajorSeventh] =
    MajorKey(tonic).map(_ => new MajorSeventh(tonic))
}

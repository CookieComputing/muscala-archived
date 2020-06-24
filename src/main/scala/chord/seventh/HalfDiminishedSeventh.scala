package chord.seventh

import chord.Chord
import chord.triad.DiminishedTriad
import interval.qualifier.Diatonic
import key.MinorKey

/**
  * Represents a half diminished seventh. A minor seventh is a diminished triad with a minor seventh interval.
  */
case class HalfDiminishedSeventh private (tonic: String)
    extends ASeventh(
      tonic,
      n => DiminishedTriad(n).get,
      n => MinorKey(n).get,
      (n, k) => Diatonic.seventh(n)(k).get
    ) {

  /**
    * Returns the half diminished seventh in string format.
    * We opt to use the jazz terminology for the half diminished seventh.
    * @return the chord in string format
    */
  override def toString: String =
    tonic + Chord.minor + Chord.seventh + Chord.flat + Chord.fifth
}

object HalfDiminishedSeventh {
  def apply(tonic: String): Option[HalfDiminishedSeventh] =
    MinorKey(tonic).map(_ => new HalfDiminishedSeventh(tonic))
}

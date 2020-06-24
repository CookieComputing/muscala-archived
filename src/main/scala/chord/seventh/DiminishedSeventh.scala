package chord.seventh

import chord.Chord
import chord.triad.{DiminishedTriad, MinorTriad}
import interval.qualifier.Diatonic
import key.MinorKey

/**
  * Represents a diminished seventh. A minor seventh is a diminished triad with a diminished seventh interval.
  */
case class DiminishedSeventh private (tonic: String)
  extends ASeventh(
    tonic,
    n => DiminishedTriad(n).get,
    n => MinorKey(n).get,
    (n, k) => Diatonic.seventh(n)(k).get.flat
  ) {

  /**
    * Returns the diminished seventh in string format.
    * @return the chord in string format
    */
  override def toString: String = triad.toString + Chord.seventh
}

object DiminishedSeventh {
  def apply(tonic: String): Option[DiminishedSeventh] =
    MinorKey(tonic).map(_ => new DiminishedSeventh(tonic))
}

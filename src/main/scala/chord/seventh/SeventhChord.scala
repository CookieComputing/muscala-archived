package chord.seventh

import chord.Chord

/**
  * Represents a seventh chord. A seventh chord takes a basic triad and adds a seventh to the chord.
  */
trait SeventhChord extends Chord {

  /**
    * The original triad of the chord (excluding the seventh).
    */
  val triad: Chord
}

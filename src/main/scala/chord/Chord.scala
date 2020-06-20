package chord

import note.Note

/**
  * A chord is a collection of notes, usually played together to create a harmony. The chord package makes extensive
  * use of the diatonic package because of how tightly coupled chords are with a key.
  */
trait Chord {
  // The notes of the chord. Order occurs from bottom-most note of the staff, upwards.
  val notes: List[Note]

  // Because the note may be inverted, it's possible that the bottom note is not the tonic
  val tonic: String

  /**
    * Represents the string version of this chord, with qualifiers attached to the end
    * @return the chord in string format
    */
  def toString: String
}

object Chord {
  // A collection of chord qualifier names
  val major = "maj"
  val minor = "min"
  val augmented = "aug"
  val diminished = "dim"
  val sharp = Note.Sharp
  val flat = Note.Flat
  val second = "2"
  val fourth = "4"
  val fifth = "5"
  val sixth = "6"
  val seventh = "7"
  val ninth = "9"
  val eleventh = "11"
  val thirteenth = "13"
  val add = "add"
  val suspended = "sus"
}

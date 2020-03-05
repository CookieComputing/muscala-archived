package key

import key.MajorKey.{
  buildKeySignature,
  flatKeySignature,
  ordering,
  sharpKeySignature
}
import note.Note

import scala.util.matching.Regex

/**
  * Represents a key. A key can contain accidentals denoting which natural notes to play
  * as sharp or flat.
  */
trait Key {
  // The tonic of the key
  val tonic: String

  // The notes in the key. In major and minor keys, there are to be seven notes in this list. These are ordered
  // starting from the tonic of the key.
  val notes: List[String]

  // The key signature containing all accidentals in a key.
  val signature: List[String] =
    notes.filter(note => note.last == Note.Flat || note.last == Note.Sharp)

  // Convert this key to a major key
  def toMajor: MajorKey

  // Convert this key to a minor key
  def toMinor: MinorKey
}

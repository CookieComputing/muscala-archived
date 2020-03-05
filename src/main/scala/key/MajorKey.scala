package key

import note.Note

/**
  * Represents a major key.
  *
  * @param tonic the tonic of the key
  */
final case class MajorKey private (tonic: String, notes: List[String])
    extends Key {

  /**
    * @return this major key
    */
  override def toMajor: MajorKey = this.copy()

  /**
    * @return the relative minor of this major key
    */
  override def toMinor: MinorKey =
    MinorKey(notes(5)).get // the minor is simply the aeolian mode of the major key
}

object MajorKey extends KeyBuilder {
  override protected val baseKey = "C"
  override protected val baseKeySignature =
    List("C", "D", "E", "F", "G", "A", "B")

  /**
    * @param tonic the tonic to create a key from
    * @return a key if the tonic is valid or none otherwise
    */
  def apply(tonic: String): Option[MajorKey] = tonic match {
    case KeyRegex() =>
      Some(
        // Follows the following heuristics for converting to another key signature
        // Sharping: Take the basic key signature, use the fifth as the new root, sharp the old 4th
        // Flatting: Take the basic key signature, use the fourth as the new root, flat the old 7th
        new MajorKey(
          tonic,
          buildKeySignature(
            baseKey,
            tonic,
            baseKeySignature,
            sharpKeySignature,
            flatKeySignature
          )
        )
      )
    case _ => None
  }

  // Example process
  // C D E F G A B
  // F G A Bb C D E
  // Bb C D Eb F G A
  private def flatKeySignature(signature: List[String]): List[String] = {
    val flattedSeventh = flatNote(signature(6))
    signature.slice(3, 6) ++ List(flattedSeventh) ++ signature.take(3)
  }

  // Example process
  // C D E F G A B
  // G A B C D E F#
  // D E F# G A B C#
  private def sharpKeySignature(signature: List[String]): List[String] = {
    val sharpedFourth = sharpNote(signature(3))
    signature.slice(4, 7) ++ signature.take(3) ++ List(sharpedFourth)
  }
}

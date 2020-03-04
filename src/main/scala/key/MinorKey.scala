package key

import note.Note

/**
 * Represents a minor key.
 */
case class MinorKey private (tonic: String, notes: List[String]) extends Key {
  /**
   * @return this major key
   */
  override def toMajor: MajorKey = MajorKey(notes(2)).get

  /**
   * @return the relative minor of this major key
   */
  override def toMinor: MinorKey = this.copy()
}

object MinorKey {
  private val KeyRegex = "^[A-G](?:#*|b*)$".r
  private val baseKey = "A"
  private val baseKeySignature = List("A", "B", "C", "D", "E", "F", "G")
  private val ordering =
    Map("F" -> 0, "C" -> 1, "G" -> 2, "D" -> 3, "A" -> 4, "E" -> 5, "B" -> 6)

  /**
   * @param tonic the tonic to create a key from
   * @return a key if the tonic is valid or none otherwise
   */
  def apply(tonic: String): Option[MinorKey] = tonic match {
    case KeyRegex() =>
      Some(new MinorKey(tonic, buildKeySignature(baseKey, tonic, baseKeySignature)))
    case _ => None
  }

  // Follows the following heuristics for converting to another key signature
  // Sharping: Take the basic key signature, use the fifth as the new root, sharp the old 6th
  // Flatting: Take the basic key signature, use the fourth as the new root, flat the old 2nd
  private def buildKeySignature(
                                 tonic: String,
                                 endTonic: String,
                                 signature: List[String]
                               ): List[String] = {
    val buildNewTonic = (buildNewSignature: (List[String] => List[String])) => {
      val newList = buildNewSignature(signature)
      buildKeySignature(newList.head, endTonic, newList)
    }
    endTonic match {
      case _ if tonic == endTonic =>
        signature // we have matched the signature, we are done
      case _ if endTonic.last == Note.Sharp => buildNewTonic(sharpKeySignature)
      case _ if endTonic.last == Note.Flat  => buildNewTonic(flatKeySignature)
      case _ if ordering(tonic) < ordering(endTonic) =>
        buildNewTonic(sharpKeySignature)
      case _ if ordering(tonic) > ordering(endTonic) =>
        buildNewTonic(flatKeySignature)
    }
  }

  // Example process
  // A B C D E F G
  // D E F G A Bb C
  // G A Bb C D Eb F
  private def flatKeySignature(signature: List[String]): List[String] = {
    val flattedSecond = flatNote(signature(1))
    signature.drop(3) ++ signature.take(1) ++ List(flattedSecond) ++ signature.slice(2,3)
  }

  // Example process
  // A B C D E F G
  // E F# G A B C D
  // B C# D E F# G A
  private def sharpKeySignature(signature: List[String]): List[String] = {
    val sharpedSixth = sharpNote(signature(5))
    signature.slice(4, 5) ++ List(sharpedSixth) ++ signature.takeRight(1) ++ signature.take(4)
  }

  private def sharpNote(note: String) =
    if (note.last == Note.Flat) note.drop(1) else note + Note.Sharp

  private def flatNote(note: String) =
    if (note.last == Note.Sharp) note.drop(1) else note + Note.Flat
}

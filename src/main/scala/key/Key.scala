package key

import note.Note

/**
  * Represents a key.
 *
 * @param tonic the tonic of the key
  */
case class Key private (tonic: String, signature: List[String]) {
}

object Key {
  private val KeyRegex = "^[A-G](?:#*|b*)$".r
  private val baseKey = "C"
  private val baseKeySignature = List("C", "D", "E", "F", "G", "A", "B")
  private val ordering = Map("F" -> 0, "C" -> 1, "G" -> 2, "D" -> 3, "A" -> 4, "E" -> 5, "B" -> 6)

  /**
   * @param tonic the tonic to create a key from
   * @return a key if the tonic is valid or none otherwise
   */
  def apply(tonic: String): Option[Key] = tonic match {
    case KeyRegex() => Some(new Key(tonic, buildKeySignature(baseKey, tonic, baseKeySignature)))
    case _ => None
  }

  // Follows the following heuristics for converting to another key signature
  // Sharping: Take the basic key signature, use the fifth as the new root, sharp the old 4th
  // Flatting: Take the basic key signature, use the fourth as the new root, sharp the old 7th
  private def buildKeySignature(tonic: String, endTonic: String, signature: List[String]): List[String] = {
    val buildNewTonic = (buildNewSignature: (List[String] => List[String])) => {
      val newList = buildNewSignature(signature)
      buildKeySignature(newList.head, endTonic, newList)
    }
    endTonic match {
      case _ if tonic == endTonic => signature // we have matched the signature, we are done
      case _ if endTonic.last == Note.Sharp => buildNewTonic(sharpKeySignature)
      case _ if endTonic.last == Note.Flat => buildNewTonic(flatKeySignature)
      case _ if ordering(tonic) < ordering(endTonic) => buildNewTonic(sharpKeySignature)
      case _ if ordering(tonic) > ordering(endTonic) => buildNewTonic(flatKeySignature)
    }
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
    signature.slice(4,7) ++ signature.take(3) ++ List(sharpedFourth)
  }

  private def sharpNote(note: String) = if (note.last == Note.Flat) note.drop(1) else note + Note.Sharp

  private def flatNote(note: String) = if (note.last == Note.Sharp) note.drop(1) else note + Note.Flat
}

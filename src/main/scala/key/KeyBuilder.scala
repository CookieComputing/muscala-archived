package key

import note.Note

import scala.util.matching.Regex

/**
  * KeyBuilder is a helper trait to construct a list of notes for a key
  */
trait KeyBuilder {
  protected val KeyRegex: Regex = "^[A-G](?:#*|b*)$".r
  protected val baseKey: String
  protected val baseKeySignature: List[String]
  protected val ordering =
    Map("F" -> 0, "C" -> 1, "G" -> 2, "D" -> 3, "A" -> 4, "E" -> 5, "B" -> 6)

  protected def buildKeySignature(
      tonic: String,
      endTonic: String,
      signature: List[String],
      sharpKeySignature: List[String] => List[String],
      flatKeySignature: List[String] => List[String]
  ): List[String] = {
    val buildNewTonic = (buildNewSignature: (List[String] => List[String])) => {
      val newList = buildNewSignature(signature)
      buildKeySignature(
        newList.head,
        endTonic,
        newList,
        sharpKeySignature,
        flatKeySignature
      )
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

  protected def sharpNote(note: String): String =
    if (note.last == Note.Flat) note.drop(1) else note + Note.Sharp

  protected def flatNote(note: String): String =
    if (note.last == Note.Sharp) note.drop(1) else note + Note.Flat
}

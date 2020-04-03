package key

import note.Note

/**
  * Represents a minor key.
  */
final case class MinorKey private (tonic: String, notes: List[String])
    extends Key {

  /**
    * @return this major key
    */
  override def toMajor: MajorKey = MajorKey(notes(2)).get

  /**
    * @return the relative minor of this major key
    */
  override def toMinor: MinorKey = this.copy()
}

object MinorKey extends KeyBuilder {
  override protected val baseKey: String = "A"
  override protected val baseKeySignature =
    List("A", "B", "C", "D", "E", "F", "G")

  /**
    * @param tonic the tonic to create a key from
    * @return a key if the tonic is valid or none otherwise
    */
  def apply(tonic: String): Option[MinorKey] = tonic match {
    case MinorKey.KeyRegex() =>
      Some(
        // Follows the following heuristics for converting to another key signature
        // Sharping: Take the basic key signature, use the fifth as the new root, sharp the old 6th
        // Flatting: Take the basic key signature, use the fourth as the new root, flat the old 2nd
        new MinorKey(
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
  // A B C D E F G
  // D E F G A Bb C
  // G A Bb C D Eb F
  private def flatKeySignature(signature: List[String]): List[String] = {
    val flattedSecond = flatNote(signature(1))
    signature.drop(3) ++ signature.take(1) ++ List(flattedSecond) ++ signature
      .slice(2, 3)
  }

  // Example process
  // A B C D E F G
  // E F# G A B C D
  // B C# D E F# G A
  private def sharpKeySignature(signature: List[String]): List[String] = {
    val sharpedSixth = sharpNote(signature(5))
    signature.slice(4, 5) ++ List(sharpedSixth) ++ signature.takeRight(1) ++ signature
      .take(4)
  }

  // convenience wrappers for natural minor keys
  def A: MinorKey = MinorKey("A").get

  def B: MinorKey = MinorKey("B").get

  def C: MinorKey = MinorKey("C").get

  def D: MinorKey = MinorKey("D").get

  def E: MinorKey = MinorKey("E").get

  def F: MinorKey = MinorKey("F").get

  def G: MinorKey = MinorKey("G").get
}

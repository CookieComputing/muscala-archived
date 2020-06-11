package key

import note.Note

/**
  * Represents a key. A key can contain accidentals denoting which natural notes to play
  * as sharp or flat.
  */
sealed trait Key {
  // The tonic of the key
  val tonic: String

  // The notes in the key. In major and minor keys, there are to be seven notes in this list. These are ordered
  // starting from the tonic of the key.
  val notes: List[String]

  // The key signature containing all accidentals in a key.
  val signature: List[String] =
    notes.filter(note => note.last == Note.Flat || note.last == Note.Sharp)

  def contains(note: Note): Boolean =
    notes.exists(keyNote =>
      Note(keyNote).get.rank % Note.HalfStepsInOctave == note.rank % Note.HalfStepsInOctave
    )

  // Convert this key to a major key
  def toMajor: MajorKey

  // Convert this key to a minor key
  def toMinor: MinorKey
}

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

  // Convenience methods for major keys
  def A: Key = MajorKey("A").get

  def B: Key = MajorKey("B").get

  def C: Key = MajorKey("C").get

  def D: Key = MajorKey("D").get

  def E: Key = MajorKey("E").get

  def F: Key = MajorKey("F").get

  def G: Key = MajorKey("G").get
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

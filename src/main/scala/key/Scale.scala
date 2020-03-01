package key

/**
 * A Scale is a repeated sequence of notes that form a cycle once exhausted
 */
trait Scale {
  val key: MajorKey



  /**
   * Returns the notes of the scale in order
   * @return a stringified version of the scale's key signature
   */
  override def toString: String = key.signature.mkString(" ")
}

package key

import note.Note

/**
 * Represents a minor key.
 */
case class MinorKey (tonic: String, notes: List[String])

object MinorKey {
  /**
   * @param tonic the tonic to create a key from
   * @return a key if the tonic is valid or none otherwise
   */
  def apply(tonic: String): Option[MinorKey] = tonic match {
    case tonic if MajorKey(tonic).isDefined => None
    case _ => None
  }
}

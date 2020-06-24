package chord.seventh

import chord.Chord
import key.Key
import note.Note

/**
  * An abstract class for constructing a seventh chord. A seventh chord is a triad,
  * followed by some sort of seventh interval.
  */
abstract class ASeventh(
    tonic: String,
    private val triadFunction: String => Chord,
    private val keyFunction: String => Key,
    private val seventh: (Note, Key) => Note
) extends SeventhChord {
  private implicit val key: Key = keyFunction(tonic)
  override val triad: Chord = triadFunction(tonic)
  override val notes: List[Note] = {
    val root = triad.notes.head
    triad.notes ++ List(seventh(root, key))
  }
}

package chord

import key.{Key, MajorKey}
import note.Note

/**
  * Abstract class for generating major and minor triads.
  * @param tonic the tonic to generate the triad from
  * @param keyFunction the major or minor key to generate with
  * @param third the type of third that should come from the chord
  */
abstract class ATriad(
    tonic: String,
    private val keyFunction: String => Key,
    private val third: (Note, Key) => Note,
    private val fifth: (Note, Key) => Note
) extends Chord {
  // One interesting observation about the absolute triads is that the triad will take the root, third, and fifth
  // of the key it is created in. This is how we calculate the specific letter associated with the note.
  private implicit val key: Key = keyFunction(tonic)

  val notes: List[Note] = {
    val baseNote = Note(key.notes.head).get
    List(
      baseNote.copy(key.notes.head),
      third(baseNote, key),
      fifth(baseNote, key)
    )
  }
}

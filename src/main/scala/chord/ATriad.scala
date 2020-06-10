package chord

import key.{Key, MajorKey}
import note.Note

abstract class ATriad(
    tonic: String,
    private val keyFunction: String => Key,
    private val third: Note => Note
) extends Chord {
  // One interesting observation about the absolute triads is that the triad will take the root, third, and fifth
  // of the key it is created in. This is how we calculate the specific letter associated with the note.
  private implicit val key: Key = keyFunction(tonic)

  val notes: List[Note] = {
    val baseNote = Note(key.notes.head).get
    List(
      baseNote.copy(key.notes.head),
      third(baseNote).copy(key.notes(2)),
      baseNote.perfect.fifth.copy(key.notes(4))
    )
  }
}

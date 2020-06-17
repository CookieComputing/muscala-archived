package chord

import key.MajorKey
import note.Note

/**
  * Represents an augmented triad. An augmented chord is defined as a major third, followed by a major third.
  */
case class AugmentedTriad private(override val tonic: String)
  extends ATriad(tonic,
    MajorKey(_).get,
    (n, k) => n.major.third.copy(note = k.notes(2)),
    (n, k) => n.perfect.fifth.sharp.copy(note = Note(k.notes(4)).get.sharp.note)) {
    override def toString: String = tonic + Chord.augmented
}

object AugmentedTriad {
    def apply(tonic: String): Option[AugmentedTriad] =
        MajorKey(tonic)
          .map(_ => new AugmentedTriad(tonic))
}

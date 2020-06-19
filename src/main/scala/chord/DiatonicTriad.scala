package chord

import key.{Key, MajorKey, MinorKey}
import note.Note

/**
  * Provides a convenient wrapper for generating a chord from a tonic given a specific key.
  */
object DiatonicTriad {

  /**
    * Creates a diatonic note.
    * @param tonic the tonic that the chord is based off of
    * @param key the key to generate the chord from
    * @return the diatonic chord if it exists, otherwise none
    */
  def apply(tonic: String)(implicit key: Key): Option[Chord] =
    for {
      note <- Note(tonic)
      _ <- Some(note).filter(n => key.contains(n))
      // Regardless of whether it's a major or minor chord, it should be defined
      _ <- Some(tonic).filter(s => MajorTriad(s).isDefined)
    } yield {
      val index = key.notes.indexOf(tonic)
      key match {
        case MajorKey(_, _) => majorKeyScaleFunction(index + 1)(tonic)
        case MinorKey(_, _) => minorKeyScaleFunction(index + 1)(tonic)
      }
    }

  // Indexed by 1 to clarify the scale degrees
  private val majorKeyScaleFunction = Map[Int, String => Chord](
    1 -> (n => MajorTriad(n).get),
    2 -> (n => MinorTriad(n).get),
    3 -> (n => MinorTriad(n).get),
    4 -> (n => MajorTriad(n).get),
    5 -> (n => MajorTriad(n).get),
    6 -> (n => MinorTriad(n).get),
    7 -> (n => DiminishedTriad(n).get)
  )

  private val minorKeyScaleFunction = Map[Int, String => Chord](
    1 -> (n => MinorTriad(n).get),
    2 -> (n => DiminishedTriad(n).get),
    3 -> (n => MajorTriad(n).get),
    4 -> (n => MinorTriad(n).get),
    5 -> (n => MinorTriad(n).get),
    6 -> (n => MajorTriad(n).get),
    7 -> (n => MajorTriad(n).get)
  )

  private def sameNoteDifferentOctave(first: Note, second: Note) =
    first.rank % Note.HalfStepsInOctave == second.rank % Note.HalfStepsInOctave

  @scala.annotation.tailrec
  private def increment(src: Note, dst: Note): Note = {
    if (sameNoteDifferentOctave(src, dst)) src else increment(src.sharp, dst)
  }
}

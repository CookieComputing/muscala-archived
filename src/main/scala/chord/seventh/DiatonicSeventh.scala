package chord.seventh

import chord.triad.DiatonicTriad.{majorKeyScaleFunction, minorKeyScaleFunction}
import key.{Key, MajorKey, MinorKey}
import note.Note

/**
  * Provides a convenient wrapper for generating a seventh chord from a tonic given a specific key.
  */
object DiatonicSeventh {
  def apply(tonic: String)(implicit key: Key): Option[SeventhChord] = for {
    note <- Note(tonic)
    _ <- Some(note).filter(n => key.contains(n))
  } yield {
    val index = key.notes.indexOf(tonic)
    key match {
      case MajorKey(_, _) => majorKeyScaleFunction(index + 1)(tonic)
      case MinorKey(_, _) => minorKeyScaleFunction(index + 1)(tonic)
    }
  }

  // Indexed by 1 to clarify the scale degrees
  private val majorKeyScaleFunction = Map[Int, String => SeventhChord](
    1 -> (n => MajorSeventh(n).get),
    2 -> (n => MinorSeventh(n).get),
    3 -> (n => MinorSeventh(n).get),
    4 -> (n => MajorSeventh(n).get),
    5 -> (n => DominantSeventh(n).get),
    6 -> (n => MinorSeventh(n).get),
    7 -> (n => HalfDiminishedSeventh(n).get)
  )

  private val minorKeyScaleFunction = Map[Int, String => SeventhChord](
    1 -> (n => MinorSeventh(n).get),
    2 -> (n => HalfDiminishedSeventh(n).get),
    3 -> (n => MajorSeventh(n).get),
    4 -> (n => MinorSeventh(n).get),
    5 -> (n => MinorSeventh(n).get),
    6 -> (n => MajorSeventh(n).get),
    7 -> (n => DominantSeventh(n).get)
  )
}

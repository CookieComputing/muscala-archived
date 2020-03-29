package interval.qualifier.absolute

import note.Note

/**
  * An IntervalQualifier is the ability to create intervals that can move a note to different pitches.
  */
object Imports {
  implicit class IntervalQualifier(val note: Note) {

    /**
      * Returns a Perfect interval qualifier for this note.
      * @return the perfect interval qualifier for this note
      */
    def perfect: Perfect = Perfect(note)

    /**
      * returns a major interval qualifier for this note.
      * @return the major interval qualifier for this note
      */
    def major: Major = Major(note)

    /**
      * Returns a Minor interval qualifier for this note.
      * @return the minor interval qualifier for this note
      */
    def minor: Minor = Minor(note)
  }
}

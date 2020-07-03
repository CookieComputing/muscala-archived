package scale

/**
  * Represents a scale. A scale is a set of notes ordered by pitch.
  */
trait Scale {

  /**
    * The notes of the scale. The order of the scale should be ordered by increasing or decreasing pitch.
    * We make no attempt to infer the number of notes in the scale since there are many types of scales that
    * break from the conventional 7 notes in the major or minor scales.
    */
  val notes: List[String]
}

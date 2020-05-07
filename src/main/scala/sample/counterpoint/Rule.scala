package sample.counterpoint

import Types._

/**
  * A rule is a condition that a counterpoint piece
  * must conform to
  */
trait Rule {
  /**
    * Applies the rule to the composition
    * @param composition the provided counterpoint composition
    * @return potential warnings and errors encountered in the composition
    */
  def apply(composition: Composition): (List[CounterpointWarning], List[CounterpointError])
}

package sample.counterpoint

import sample.counterpoint.Types._

/**
  * A checker that ensures a composition obeys the rules of a cantus firmus
  * as detailed from the Open Music theory website: http://openmusictheory.com/cantusFirmus.html
  */
class CantusFirmusChecker extends CounterpointChecker {
  val rules: List[Rule] = List()

  def analyze(composition: Composition): Either[(List[CounterpointWarning], List[CounterpointError]), Unit] = ???
}

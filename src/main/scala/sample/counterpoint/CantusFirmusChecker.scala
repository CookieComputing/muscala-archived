package sample.counterpoint

import sample.counterpoint.Types._
import sample.counterpoint.rules.CantusFirmusRules._

/**
  * A checker that ensures a composition obeys the rules of a cantus firmus
  * as detailed from the Open Music theory website: http://openmusictheory.com/cantusFirmus.html
  */
case class CantusFirmusChecker() extends CounterpointChecker {
  val rules: List[Rule] = List(singleVoicing,
    cantusFirmusLength,
    beginAndEndOnTonic)
}

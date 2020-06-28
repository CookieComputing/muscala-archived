package interval.movement.absolute

import helpers.PropertyTesting
import interval.movement.NHalfSteps
import note.Note
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Unit tests fot the NHalfStep class
  */
class NHalfStepTest extends FunSuite with ScalaCheckPropertyChecks{
  test(
    "NHalfStep should appropriately move a distance of n half steps away from the original note"
  ) {
    forAll(PropertyTesting.noteGen) {
      note =>
        // Note that NHalfSteps is pretty inefficient, so large arbitrary values are hard to test quickly
        forAll(Gen.chooseNum(-1000, 1000)) {
          randomDistance =>
            assert(note.distance(note.nHalfSteps(randomDistance).up) == randomDistance)
            assert(note.distance(note.nHalfSteps(randomDistance).down) == -randomDistance)
        }
    }
  }
}

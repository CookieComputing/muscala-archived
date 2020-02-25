package interval.movement

import note.Note
import org.scalatest.FunSuite

import scala.util.Random

/**
 * Unit tests fot the NHalfStep class
 */
class NHalfStepTest extends FunSuite {
  test("A note should be extractable from a NHalfStep") {
    Note.A.nHalfSteps(3) match {
      case NHalfSteps(note, interval) => assert(note == Note.A && interval == 3)
      case _ => assert(false, "expected half step when calling halfStep() method")
    }
  }

  test("NHalfStep should appropriately move a distance of n half steps away from the original note") {
    val fuzzer = Random
    for (_ <- 1 to 10) {
      val randomDistance = fuzzer.nextInt(10)
      val alteredNote = Note.A.nHalfSteps(randomDistance)

      assert(alteredNote.down.distance(Note.A) == randomDistance)
      assert(alteredNote.up.distance(Note.A) == -randomDistance)
    }
  }
}

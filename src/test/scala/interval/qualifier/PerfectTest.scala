package interval.qualifier

import note.Note
import org.scalatest.FunSuite


/**
 * Represents unit tests for the perfect interval qualifer
 */
class PerfectTest extends FunSuite {
  test("fifth should work as expected") {
    assert(Note("A").get.perfect.fifth.toString == "E")
  }
}

package interval.qualifier

import note.Note
import org.scalatest.FunSuite

/**
 * Represents unit tests for the minor interval qualifer
 */
class MinorTest extends FunSuite {
  test("third should work as expected") {
    assert(Note("A").get.minor.third.toString == "C")
  }
}

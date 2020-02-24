package interval.qualifier

import helpers.NoteTesting
import note.Note
import org.scalatest.FunSuite

/**
 * Represents unit tests for the major interval qualifer
 */
class MajorTest extends FunSuite {
  test("third should work as expected") {
    assert(Note("A").get.major.third.toString == "C#")
  }
}

package sample.counterpoint.rule

import helpers.NoteTesting
import note.Note
import org.scalatest.FunSuite
import sample.counterpoint.rules.CantusFirmusRules
import sample.counterpoint.rules.CantusFirmusRules._
import _root_.key.MajorKey

/**
  * Tests to ensure a cantus firmus composition obeys all rules
  */
class CantusFirmusRulesTest extends FunSuite {
  test("A cantus firmus should have a single voicing") {
    assert(singleVoicing((List[List[Note]](List(Note.A)), MajorKey.A)).isRight)
    assert(singleVoicing((List[List[Note]](), MajorKey.A)).isLeft)
    assert(singleVoicing((List[List[Note]](List(Note.A), List(Note.B)), MajorKey.A)).isLeft)
    assert(singleVoicing((List[List[Note]](), MajorKey.A)).swap.getOrElse(Nil)
      == (Nil, List(CantusFirmusRules.cantusFirmusSingleVoicing)))
  }

  test("A cantus firmus should have 8 - 16 notes in length") {
    assert(cantusFirmusLength((List(NoteTesting.toNoteSeq((0 until 8).map(_ => "A").toList: _*)), MajorKey.A)).isRight)
    assert(cantusFirmusLength((List(NoteTesting.toNoteSeq((0 until 16).map(_ => "A").toList: _*)), MajorKey.A)).isRight)
    assert(cantusFirmusLength((List(NoteTesting.toNoteSeq((0 until 7).map(_ => "A").toList: _*)), MajorKey.A)).isLeft)
    assert(cantusFirmusLength((List(NoteTesting.toNoteSeq((0 until 17).map(_ => "A").toList: _*)), MajorKey.A)).isLeft)

    assert(cantusFirmusLength((List(NoteTesting.toNoteSeq((0 until 7).map(_ => "A").toList: _*)), MajorKey.A))
      .swap.getOrElse(Nil) == (Nil, List(CantusFirmusRules.cantusFirmusInvalidLength)))
  }

  test("A cantus firmus should begin and end on the tonic, in the same register") {
    assert(beginAndEndOnTonic((List(NoteTesting.toNoteSeq("A", "A")), MajorKey.A)).isRight)
    assert(beginAndEndOnTonic((List(NoteTesting.toNoteSeq("B", "D", "C", "A", "B")), MajorKey.B)).isRight)
    assert(beginAndEndOnTonic((List(NoteTesting.toNoteSeq("B", "D", "C", "A", "A")), MajorKey.B)).isLeft)
    assert(beginAndEndOnTonic((List(NoteTesting.toNoteSeq("B", "D", "C", "A", "B")), MajorKey.C)).isLeft)
    assert(beginAndEndOnTonic((List(NoteTesting.toNoteSeq("A", "B")), MajorKey.A)).isLeft)
    assert(beginAndEndOnTonic((List(NoteTesting.toNoteSeq("A", "B")), MajorKey.A)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusInvalidTonicEdges)))
    assert(beginAndEndOnTonic((List(Seq(Note("A", 4).get, Note("A", 5).get)), MajorKey.A)).isLeft)
  }

  test("A cantus firmus should approach the final tonic by step") {
  }
}

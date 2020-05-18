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
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("A", "B", "C", "D", "B", "A")), MajorKey.A)).isRight)
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("G#", "A")), MajorKey.A)).isRight)
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("A#", "B")), MajorKey.B)).isRight)
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("C#", "B")), MajorKey.B)).isRight)
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("A", "B")), MajorKey.B)).isLeft)
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("A", "B")), MajorKey.B)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusInvalidFinalStepApproach)))
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("A#b", "B")), MajorKey.B)).isLeft)
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("A#b", "B")), MajorKey.B)).swap.getOrElse(Nil)
    == (Nil, List(cantusFirmusInvalidFinalStepApproach)))
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("C", "B")), MajorKey.B)).isLeft)
    assert(approachFinalTonicByStep((
      List(NoteTesting.toNoteSeq("C", "B")), MajorKey.B)).swap.getOrElse(Nil)
    == (Nil, List(cantusFirmusInvalidFinalStepApproach)))
  }

  test("A cantus firmus should only have melodic consonances") {
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "D", "E", "F", "G", "A", "B")), MajorKey.C)).isRight)
    // Major/minor second
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "D")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "Db")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("D", "C")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("D", "C")), MajorKey.C)).isRight)
    // Major/minor third
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "E")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "Eb")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("E", "C")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("Eb", "C")), MajorKey.C)).isRight)
    // Major/minor sixth
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "A")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "Ab")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("A", "C")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("Ab", "C")), MajorKey.C)).isRight)
    // Perfect fourth
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "F")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("F", "C")), MajorKey.C)).isRight)
    // Perfect fifth
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "G")), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("G", "C")), MajorKey.C)).isRight)
    // Perfect Octave
    assert(melodicConsonancesOnly((
      List(List(Note("C",4).get, Note("C", 5).get)), MajorKey.C)).isRight)
    assert(melodicConsonancesOnly((
      List(List(Note("C",5).get, Note("C", 4).get)), MajorKey.C)).isRight)

    // Some dissonant values - Does not check enharmonically dissonant values
    // like augmented unions to minor seconds
    // Augmented fourth
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "F#")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("F#", "C")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "F#")), MajorKey.C)).swap.getOrElse(Nil)
    == (Nil, List(cantusFirmusDissonantMelodicInterval.format("C", "F#"))))
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("F#", "C")), MajorKey.C)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusDissonantMelodicInterval.format("F#", "C"))))
    // Diminished fifth
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "Gb")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "Gb")), MajorKey.C)).swap.getOrElse(Nil)
    == (Nil, List(cantusFirmusDissonantMelodicInterval.format("C", "Gb"))))
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("Gb", "C")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("Gb", "C")), MajorKey.C)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusDissonantMelodicInterval.format("Gb", "C"))))
    // Sevenths - major/minor
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "B")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "B")), MajorKey.C)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusDissonantMelodicInterval.format("C", "B"))))
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "Bb")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "Bb")), MajorKey.C)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusDissonantMelodicInterval.format("C", "Bb"))))
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("B", "C")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("B", "C")), MajorKey.C)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusDissonantMelodicInterval.format("B", "C"))))
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("Bb", "C")), MajorKey.C)).isLeft)
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("Bb", "C")), MajorKey.C)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusDissonantMelodicInterval.format("Bb", "C"))))

    // This rule additionally covers a rule that no unison notes are allowed for repetition
    assert(melodicConsonancesOnly((
      List(NoteTesting.toNoteSeq("C", "C")), MajorKey.C)).swap.getOrElse(Nil)
      == (Nil, List(cantusFirmusDissonantMelodicInterval.format("C", "C"))))

  }

  test("A cantus firmus should not outline any dissonant notes") {
    // TODO: implement
  }

  test("A cantus firmus should have a range of a tenth or less") {
    assert(doesNotExceedTenth((List(NoteTesting.toNoteSeq("C", "D", "E", "F", "G", "A", "B")), MajorKey.C))
      .isRight)
    assert(doesNotExceedTenth((
      List(List(Note("C", 4).get, Note("C", 5).get, Note("D", 5).get, Note("Eb", 5).get, Note("E", 5).get)),
      MajorKey.C)).isRight)
    assert(doesNotExceedTenth((
      List(List(Note("C", 4).get, Note("E#", 5).get)),
      MajorKey.C)).isLeft)
    assert(doesNotExceedTenth((
      List(List(Note("C", 4).get, Note("E#", 5).get)),
      MajorKey.C)).swap.getOrElse(Nil)
    == (Nil, List(cantusFirmusDoesNotExceedTenth)))
  }

  test("A cantus firmus should have just a single climax") {
    assert(singleClimax((List(NoteTesting.toNoteSeq("C", "D", "E", "F", "G", "F", "E", "D", "C")), MajorKey.C))
      .isRight)
    // We can almost reach the climax but not quite touch it, and the composition is still valid
    assert(singleClimax((List(NoteTesting.toNoteSeq("C", "D", "E", "D", "C", "D")), MajorKey.C))
      .isRight)
    assert(singleClimax((List(NoteTesting.toNoteSeq("C", "D", "E", "D", "C", "D", "E")), MajorKey.C))
      .isLeft)
    assert(singleClimax((List(NoteTesting.toNoteSeq("C", "D", "E", "D", "C", "D", "E")), MajorKey.C))
      .swap.getOrElse(Nil) == (Nil, List(cantusFirmusSingleClimax)))
  }

  test("A cantus firmus should have mostly stepwise motion") {
    assert(mostlyStepwiseMotion((List(NoteTesting.toNoteSeq("C", "D", "E", "F", "G", "F", "E", "D", "C")), MajorKey.C))
      .isRight)
    assert(mostlyStepwiseMotion((List(NoteTesting.toNoteSeq("C", "E", "C", "E", "C", "E", "C", "E", "C")), MajorKey.C))
      .isLeft)
    assert(mostlyStepwiseMotion((List(NoteTesting.toNoteSeq("C", "E", "C", "E", "C", "E", "C", "E", "C")), MajorKey.C))
      .swap.getOrElse(Nil) == (List(cantusFirmusManyLeaps.format(8)), Nil))
  }

  test("A cantus firmus should have a contrary stepwise motion counteracting a leap >= fourth") {
    assert(largeLeapsAreCounteractedByContraryStepwiseMotion((List(NoteTesting.toNoteSeq("C", "D")), MajorKey.C))
      .isRight)
    assert(largeLeapsAreCounteractedByContraryStepwiseMotion((List(NoteTesting.toNoteSeq("C", "E")), MajorKey.C))
      .isRight)
    assert(largeLeapsAreCounteractedByContraryStepwiseMotion((List(NoteTesting.toNoteSeq("C", "F")), MajorKey.C))
      .isLeft)
    // TODO: Add more complex tests to this function
  }
}

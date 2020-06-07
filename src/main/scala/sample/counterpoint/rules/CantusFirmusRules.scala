package sample.counterpoint.rules

import sample.counterpoint.Types._
import note.Note

/**
  * A collection of cantus firmus rules on a composition. The ordering of the
  * rules are important as some rules may rely on the previous rules enforcing some invariant.
  * Rules are only checking for the specific property they are required to satisfy. This
  * means a rule out of context can result in strange behavior like out of bounds access.
  *
  * Differing opinions exist for a few niche rules (although there are a few rules conforming ot the general consensus):
  * This project uses Open Music Theory's interpretation of a "cantus firmus" since it provides a more concrete
  * definition for a cantus firmus: http://openmusictheory.com/cantusFirmus.html
  */
case object CantusFirmusRules {
  val cantusFirmusSingleVoicing: Error = "cantus firmus should have one voicing"
  val cantusFirmusInvalidLength: Error =
    "cantus firmus should have 8 - 16 notes"
  val cantusFirmusInvalidTonicEdges: Error =
    "cantus firmus should start and end on tonic, in same register"
  val cantusFirmusInvalidFinalStepApproach: Error =
    "cantus firmus should approach final tonic by step"
  val cantusFirmusDissonantMelodicInterval: Error =
    "found bad interval: %s -> %s is a dissonant interval"
  val cantusFirmusDoesNotExceedTenth: Error =
    "cantus firmus should not exceed a tenth in range"
  val cantusFirmusSingleClimax: Error =
    "cantus firmus should have a single climax"
  val cantusFirmusManyLeaps: Warning =
    "cantus firmus should have <= 4 leaps, found %d leaps"
  // Third format is an optional note
  val cantusFirmusLeapNotCounteractedByStep: Error =
    "cantus firmus leap not counteracted by contrary step: %s -> %s%s"
  val cantusFirmusGreaterThanTwoLeaps: Error =
    "cantus firmus more than two leaps"

  /**
    * Ensure that the composition has a single voicing
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def singleVoicing(composition: Composition): Either[CompIssues, Unit] =
    if (composition._1.length == 1) Right()
    else Left(Nil, List(cantusFirmusSingleVoicing))

  /**
    * Ensure that the composition has an appropriate length
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def cantusFirmusLength(composition: Composition): Either[CompIssues, Unit] =
    if (8 <= voice(composition).length && voice(composition).length <= 16)
      Right()
    else Left(Nil, List(cantusFirmusInvalidLength))

  /**
    * Ensure that the composition starts and ends on the same tonic
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def beginAndEndOnTonic(composition: Composition): Either[CompIssues, Unit] = {
    val melody = voice(composition)
    val key = getKey(composition)
    if (melody.head == melody.last && melody.head.note == key.tonic && melody.last.note == key.tonic)
      Right()
    else Left(Nil, List(cantusFirmusInvalidTonicEdges))
  }

  /**
    * Ensure that the composition approaches the final tonic by a tonic step
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def approachFinalTonicByStep(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    val key = getKey(composition)
    if (List(key.notes(1), key.notes.last)
          .contains(voice(composition).dropRight(1).last.note)) Right()
    else Left(Nil, List(cantusFirmusInvalidFinalStepApproach))
  }

  /**
    * Ensure that the composition only follows melodic consonances between notes. According to Open Music Theory
    * (http://openmusictheory.com/intervals.html), the following intervals are permitted:
    * - Perfect intervals
    * - Diatonic steps (major/minor seconds)
    * - Major/minor thirds
    * - Major/minor sixths
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def melodicConsonancesOnly(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    // TODO: Some incredibly semantic rules may not work (e.g. an augmented unison),
    //  until full support for diatonic intervals is here. General support using absolute intervals should work.
    def equalDistance(n1: Note, n2: Note, qualifier: Note) =
      math.abs(n1.distance(n2)) == math.abs(n1.distance(qualifier))
    val melody = voice(composition)
    melody.drop(1).foldLeft((List.empty[Error], melody.take(1).head)) {
      (acc, note) =>
        {
          val (errors, baseNote) = acc
          if (equalDistance(baseNote, note, baseNote.perfect.fourth) ||
              equalDistance(baseNote, note, baseNote.perfect.fifth) ||
              equalDistance(baseNote, note, baseNote.perfect.octave) ||
              equalDistance(baseNote, note, baseNote.major.second) ||
              equalDistance(baseNote, note, baseNote.minor.second) ||
              equalDistance(baseNote, note, baseNote.major.third) ||
              equalDistance(baseNote, note, baseNote.minor.third) ||
              equalDistance(baseNote, note, baseNote.major.sixth) ||
              equalDistance(baseNote, note, baseNote.minor.sixth))
            (errors, note)
          else
            (
              errors ++ List(
                cantusFirmusDissonantMelodicInterval
                  .format(baseNote.note, note.note)
              ),
              note
            )
        }
    } match {
      case (Nil, _)    => Right()
      case (errors, _) => Left((Nil, errors))
    }
  }

  /**
    * Ensure that the composition does not outline a dissonant interval. Outlining refers to a string of
    * consecutively ascending or descending intervals, with the summation of the intervals representing a result.
    * This was a rule not technically listed in Open Music Theory, but is prevalent in other sources.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def doesNotOutlineDissonances(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    ???
  }

  /**
    * Ensure that the composition does not exceed a range of a tenth from the lowest pitch to the highest pitch.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def doesNotExceedTenth(composition: Composition): Either[CompIssues, Unit] = {
    val melody = voice(composition)
    val lowestNote = melody minBy { _.rank }
    val highestNote = melody maxBy { _.rank }
    if (lowestNote.distance(highestNote) <= lowestNote.distance(
          lowestNote.major.third.perfect.octave
        ))
      Right()
    else Left((Nil, List(cantusFirmusDoesNotExceedTenth)))
  }

  /**
    * Ensure that the composition has only a single high point (the highest note exists only once)
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def singleClimax(composition: Composition): Either[CompIssues, Unit] = {
    val melody = voice(composition)
    val highestNote = melody maxBy { _.rank }
    if (melody.count(n => n.rank == highestNote.rank) > 1)
      Left((Nil, List(cantusFirmusSingleClimax)))
    else Right()
  }

  /**
    * Ensure that the composition generally follows an order of stepwise motion.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def mostlyStepwiseMotion(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    // A rough guess on the number of "leaps" permissible is found here: https://www.youtube.com/watch?v=8gqL_WDeiI0
    val melody = voice(composition)
    val leaps = melody
      .foldLeft((melody.head, 0)) { (acc, note) =>
        {
          val (baseNote, leaps) = acc
          if (math.abs(baseNote.distance(note)) > math.abs(
                baseNote.distance(
                  baseNote.major.second
                )
              )) {
            (note, leaps + 1)
          } else (note, leaps)
        }
      }
      ._2
    if (leaps > 4) {
      Left(List(cantusFirmusManyLeaps.format(leaps)), Nil)
    } else {
      Right()
    }
  }

  /**
    * Ensure that the composition does not repeat any "motifs" or "licks".
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def noMotifRepetition(composition: Composition): Either[CompIssues, Unit] = {
    ???
  }

  /**
    * Ensure that large leaps (fourth or larger) are followed by step in opposite direction
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def largeLeapsAreCounteractedByContraryStepwiseMotion(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    // TODO: Refactor
    val melody = voice(composition)
    def perfectFourthOrGreater(baseNote: Note, comparisonNote: Note) =
      math.abs(baseNote.distance(comparisonNote)) >= math.abs(
        baseNote.distance(baseNote.perfect.fourth)
      )
    def checkStepUp(baseNote: Note, comparisonNote: Note): Boolean = {
      // One step up according to the key
      val keyNotes = getKey(composition).notes.map { n =>
        Note(n).get
      }
      val compare = for {
        _ <- Some(
          math.abs(baseNote.distance(comparisonNote)) > math
            .abs(baseNote.distance(baseNote.major.second))
        )
        matchingBaseNote <- keyNotes
          .find(n =>
            n.rank % Note.HalfStepsInOctave == baseNote.rank % Note.HalfStepsInOctave
          )
        matchingComparisonNote <- keyNotes
          .find(n =>
            n.rank % Note.HalfStepsInOctave == comparisonNote.rank % Note.HalfStepsInOctave
          )
        res <- Some(
          matchingBaseNote.distance(matchingComparisonNote) == matchingBaseNote
            .distance(matchingBaseNote.major.second) ||
            matchingBaseNote
              .distance(matchingComparisonNote) == matchingBaseNote
              .distance(matchingBaseNote.minor.second)
        )
      } yield res
      compare.getOrElse(false)
    }

    def formatCounteractError(first: Note, second: Note, third: Option[Note]) =
      cantusFirmusLeapNotCounteractedByStep.format(
        first.note,
        second.note,
        third.map(" -> " + _.note).getOrElse("")
      )

    def counteract(melody: Seq[Note]): List[Error] = {
      melody match {
        case first +: second +: Nil if perfectFourthOrGreater(first, second) =>
          List(formatCounteractError(first, second, None))
        case first +: second +: third +: rest
            if perfectFourthOrGreater(first, second) =>
          val counteractedByContraryStep =
            if (first.distance(second) > 0) checkStepUp(third, second)
            else checkStepUp(second, third)
          if (counteractedByContraryStep) counteract(third +: rest)
          else
            formatCounteractError(first, second, Some(third)) :: counteract(
              third +: rest
            )
        // Not a perfect fourth or greater
        case _ +: _ +: rest => counteract(rest)
        case _              => Nil
      }
    }

    val result = counteract(melody)
    if (result.isEmpty) Right() else Left(Nil, result)
  }

  /**
    * Ensure that the composition has no more than two leaps in a row.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def twoConsecutiveLeapsOrLess(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    val melody = voice(composition)
    if (melody
          .drop(1)
          .foldLeft((melody.take(1).head, 0, false)) { (acc, curNote) =>
            {
              val (prevNote, leaps, error) = acc
              if (math.abs(prevNote.distance(curNote)) > math.abs(
                    prevNote.distance(prevNote.major.second)
                  )) {
                if (leaps == 2) (curNote, leaps + 1, true)
                else (curNote, leaps + 1, error)
              } else (curNote, 0, error)
            }
          }
          ._3) Left(Nil, List(cantusFirmusGreaterThanTwoLeaps))
    else Right()
  }

  /**
    * Ensure that the leading tone progresses to the tonic.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def leadingToneProgressesToTonic(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    ???
  }

  /**
    * Ensure that the leading tone only appears in the penultimate bar for minor keys.
    * @param composition
    * @return
    */
  def leadingToneOnlyAppearsInPenultimateBarInMinor(
      composition: Composition
  ): Either[CompIssues, Unit] = {
    ???
  }

  private def voice(composition: Composition) = composition._1.head
  private def getKey(composition: Composition) = composition._2
}

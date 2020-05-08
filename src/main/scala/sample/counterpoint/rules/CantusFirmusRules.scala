package sample.counterpoint.rules

import sample.counterpoint.Types._
import interval.movement.absolute.Imports._

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
  val cantusFirmusInvalidLength: Error = "cantus firmus should have 8 - 16 notes"
  val cantusFirmusInvalidTonicEdges: Error = "cantus firmus should start and end on tonic, in same register"
  val cantusFirmusInvalidFinalStepApproach: Error = "cantus firmus should approach final tonic by step"

  /**
    * Ensure that the composition has a single voicing
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def singleVoicing(composition: Composition): Either[(List[Warning], List[Error]), Unit] =
    if (composition._1.length == 1) Right()
    else Left(Nil, List(cantusFirmusSingleVoicing))

  /**
    * Ensure that the composition has an appropriate length
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def cantusFirmusLength(composition: Composition): Either[(List[Warning], List[Error]), Unit] =
    if (8 <= voice(composition).length && voice(composition).length <= 16) Right()
    else Left(Nil, List(cantusFirmusInvalidLength))

  /**
    * Ensure that the composition starts and ends on the same tonic
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def beginAndEndOnTonic(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    val melody = voice(composition)
    val key = getKey(composition)
    if (melody.head == melody.last && melody.head.note == key.tonic && melody.last.note == key.tonic) Right()
    else Left(Nil, List(cantusFirmusInvalidTonicEdges))
  }

  /**
    * Ensure that the composition approaches the final tonic by a tonic step
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def approachFinalTonicByStep(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    val key = getKey(composition)
    if (List(key.notes(1), key.notes.last).contains(voice(composition).dropRight(1).last.note)) Right()
    else Left(Nil, List(cantusFirmusInvalidFinalStepApproach))
  }

  /**
    * Ensure that the composition only follows melodic consonances between notes. According to Open Music Theory
    * (http://openmusictheory.com/intervals.html), the following intervals are permitted:
    * - Perfect intervals
    * - Major/minor thirds
    * - Major/minor sixths
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def melodicConsonancesOnly(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the composition does not outline a dissonant interval. Outlining refers to a string of
    * consecutively ascending or descending intervals, with the summation of the intervals representing a result.
    * This was a rule not technically listed in Open Music Theory, but is prevalent in other sources.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def doesNotOutlineDissonances(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the composition does not exceed a range of a tenth from the lowest pitch to the highest pitch.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def doesNotExceedTenth(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the composition has only a single high point (the highest note exists only once)
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def singleClimax(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the composition generally follows an order of stepwise motion.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def mostlyStepwiseMotion(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the composition does not repeat any "motifs" or "licks".
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def noMotifRepetition(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that large leaps (fourth or larger) are followed by step in opposite direction
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def largeLeapsAreCounteractedByContraryStepwiseMotion(composition: Composition):
  Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the composition has no more than two leaps in a row.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def twoConsecutiveLeapsOrLess(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the leading tone progresses to the tonic.
    * @param composition the provided composition
    * @return an either indicating success or a tuple with warnings and errors
    */
  def leadingToneProgressesToTonic(composition: Composition): Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  /**
    * Ensure that the leading tone only appears in the penultimate bar for minor keys.
    * @param composition
    * @return
    */
  def leadingToneOnlyAppearsInPenultimateBarInMinor(composition: Composition):
  Either[(List[Warning], List[Error]), Unit] = {
    ???
  }

  private def voice(composition: Composition) = composition._1.head
  private def getKey(composition: Composition) = composition._2
}

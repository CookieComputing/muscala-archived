package sample.counterpoint.rules

import sample.counterpoint.Types._

/**
  * A collection of cantus firmus rules on a composition. The ordering of the
  * rules are important as some rules may rely on the previous rules enforcing some invariant.
  * Rules are only checking for the specific property they are required to satisfy. This
  * means a rule out of context can result in strange behavior like out of bounds access.
  */
case object CantusFirmusRules {
  val cantusFirmusSingleVoicing: Error = "cantus firmus should have one voicing"
  val cantusFirmusInvalidLength: Error = "cantus firmus should have 8 - 16 notes"
  val cantusFirmusInvalidTonicEdges: Error = "cantus firmus should start and end on tonic, in same register"

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
    ???
  }

  private def voice(composition: Composition) = composition._1.head
  private def getKey(composition: Composition) = composition._2
}

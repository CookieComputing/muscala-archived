package sample.counterpoint

import Types._

/**
  * A Counterpoint checker applies a list of rules to a composition and determines if the composition is valid.
  */
trait CounterpointChecker {
  val rules: List[Rule]

  /**
    * Analyzes the composition and returns a list of errors and warnings first encountered by the composition.
    * If only warnings persist, the checker will continue analyzing the rest of the piece, whereas the checker will
    * stop on the first error encountered.
    * @param composition The provided counterpoint composition
    * @return An either indicating that nothing is wrong, or a tuple of warnings and errors
    *         encountered in the composition
    */
  def analyze(
      composition: Composition
  ): Either[(List[Warning], List[Error]), Unit] = {
    rules.foldLeft((List.empty[Warning], List.empty[Error]))((acc, rule) => {
      if (acc._2.isEmpty) {
        val result = rule(composition).swap.getOrElse((Nil, Nil))
        (acc._1 ++ result._1, acc._2 ++ result._2)
      } else acc
    })
  } match {
    case (Nil, Nil) => Right()
    case issues     => Left(issues)
  }
}

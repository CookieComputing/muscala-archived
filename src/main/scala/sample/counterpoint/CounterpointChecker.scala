package sample.counterpoint

import Types._

/**
  * A Counterpoint checker applies a list of rules to a composition and determines if the composition is valid.
  */
trait CounterpointChecker {
  val rules: List[Rule]

  /**
    * @param composition The provided counterpoint composition
    * @return An either indicating that nothing is wrong, or a tuple of warnings and errors
    *         encountered in the composition
    */
  def analyze(
      composition: Composition
  ): Either[(List[Warning], List[Error]), Unit] = {
    rules.foldLeft((List[Warning](), List[Error]()))((acc, rule) => {
      rule(composition) match {
        case Left((warnings, errors)) => (acc._1 ++ warnings, acc._2 ++ errors)
        case _                        => acc
      }
    }) match {
      case (Nil, Nil) => Right()
      case errors     => Left(errors)
    }
  }
}

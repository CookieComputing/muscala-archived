package sample.counterpoint

import Types._

/**
  * An abstract class containing the general logic for a counterpoint checker
  */
abstract class Checker extends CounterpointChecker {
  def analyze(composition: Composition): Either[(List[CounterpointWarning], List[CounterpointError]), Unit] = {
    rules.foldLeft((List[CounterpointWarning](), List[CounterpointError]()))((acc, rule) => {
      val (warnings, errors) = rule.apply(composition)
      (warnings ++ acc._1, errors ++ acc._2)
    })
  } match {
    case (Nil, Nil) => Right(Unit)
    case x => Left(x)
  }
}

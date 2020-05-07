package sample.counterpoint

import note.Note

object Types {
  // In this sample repo, a composition is simply a collection of sequences.
  type Composition = List[Seq[Note]]

  // A warning or suggestion on how a counterpoint piece should be played
  type CounterpointWarning = String
  // An error in the counterpoint composition
  type CounterpointError = String
}


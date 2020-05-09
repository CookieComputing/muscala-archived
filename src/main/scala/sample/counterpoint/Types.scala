package sample.counterpoint

import key.Key
import note.Note

object Types {
  // In this sample repo, a composition is simply a collection of sequences.
  type Composition = (List[Seq[Note]], Key)

  // A warning or suggestion on how a counterpoint piece should be played
  type Warning = String
  // An error in the counterpoint composition
  type Error = String

  // Convenience type for creating a list of issues found with a composition
  type CompIssues = (List[Warning], List[Error])

  // A rule can correctly evaluate to nothing or a list of issues encountered in the stream.
  type Rule = Composition => Either[CompIssues, Unit]
}


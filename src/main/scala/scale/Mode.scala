package scale

import key.MajorKey

/**
  * Represents a mode. Technically, a mode is a type of musical scale derived from the major key, but with a
  * different root position. It's easier to reason about a mode as key with a unique characteristic.
  */
sealed trait Mode extends Scale {}

/**
  * Abstract mode class to abstract away the construction of a mode.
  * @param key The original key to convert to a mode
  */
abstract class AMode(key: MajorKey, private val rootIndex: Int) extends Mode {
  val notes: List[String] = {
    val originalNotes = key.notes
    originalNotes.slice(rootIndex, originalNotes.length) ++ originalNotes.slice(
      0,
      rootIndex
    )
  }
}

/**
  * Represents the Ionian mode.
  * @param key The original key to convert to a mode
  */
case class Ionian(key: MajorKey) extends AMode(key, 0)

/**
  * Represents the Dorian mode.
  * @param key The original key to convert to a mode
  */
case class Dorian(key: MajorKey) extends AMode(key, 1)

/**
  * Represents the Phrygian mode.
  * @param key The original key to convert to a mode
  */
case class Phrygian(key: MajorKey) extends AMode(key, 2)

/**
  * Represents the Lydian mode.
  * @param key The original key to convert to a mode
  */
case class Lydian(key: MajorKey) extends AMode(key, 3)

/**
  * Represents the Mixolydian mode.
  * @param key The original key to convert to a mode
  */
case class Mixolydian(key: MajorKey) extends AMode(key, 4)

/**
  * Represents the Aeolian mode.
  * @param key The original key to convert to a mode
  */
case class Aeolian(key: MajorKey) extends AMode(key, 5)

/**
  * Represents the Locrian mode.
  * @param key The original key to convert to a mode
  */
case class Locrian(key: MajorKey) extends AMode(key, 6)

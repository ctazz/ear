package org.ear

//TODO I should probably have enums for notes, because I don't have any way to show root movement, and I'd like to print out
//the root distance from one test chord to another

sealed trait Voicing
case object RootPostion extends Voicing
case object FirstInversion extends Voicing
case object SecondInversion extends Voicing

sealed trait Note
case object C extends Note
case object CSharp extends Note
case object D extends Note
case object DSharp extends Note
case object E extends Note
case object F extends Note
case object FSharp extends Note
case object G extends Note
case object GSharp extends Note
case object A extends Note
case object BFlat extends Note //I like BFlat here for some reason. I know names really depend on what key you're in, but not worrying about that here
case object B extends Note

//Starting to try out Notes as Enumeration, but haven't done anything with this yet
object Nte extends Enumeration {
  type Nte = Value
  val C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, BFlat, B = Value
}

sealed trait ChordType
case object Major extends ChordType
case object Minor extends ChordType

case class Description(root: Note, chordType: ChordType, voicing: Voicing)

object Music {
  val offsetData: Map[Note, Int] = Map(C -> 0, CSharp -> 1, D -> 2, DSharp -> 3, E -> 4, F -> 5, FSharp -> 6, G -> 7, GSharp -> 8, A -> 9, BFlat -> 10, B -> 11 )
  def noteOffset(note: Note): Int = {
    offsetData(note)
  }

  def classifyTheNote(note: Int): Note = {
    note match {
      case n if n % 12 == 0 => C
      case n if n % 12 == 1 => CSharp
      case n if n % 12 == 2 => D
      case n if n % 12 == 3 => DSharp
      case n if n % 12 == 4 => E
      case n if n % 12 == 5 => F
      case n if n % 12 == 6 => FSharp
      case n if n % 12 == 7 => G
      case n if n % 12 == 8 => GSharp
      case n if n % 12 == 9 => A
      case n if n % 12 == 10 => BFlat
      case n if n % 12 == 11 => B
      case other => throw new RuntimeException(s"I thought there were only 12 possibilites in mod 12. But this number broke that rule: $other")
        //Actually a negative number mod 12 is a negative number. So try to make the numbers you pass in here > 0!
    }
  }

  def nameTheNote(note: Int): String = {
    note match {
      case n if n % 12 == 0 => "C"
      case n if n % 12 == 1 => "C#"
      case n if n % 12 == 2 => "D"
      case n if n % 12 == 3 => "D#"
      case n if n % 12 == 4 => "E"
      case n if n % 12 == 5 => "F"
      case n if n % 12 == 6 => "F#"
      case n if n % 12 == 7 => "G"
      case n if n % 12 == 8 => "G#"
      case n if n % 12 == 9 => "A"
      case n if n % 12 == 10 => "BFlat"  //I like BFlat here for some reason. I know names really depend on what key you're in, but not worrying about that here
      case n if n % 12 == 11 => "B"
    }
  }
  val whiteKeys = Set[Note](C,D, E, F, G, A, B, C)

  def isCorrectRoot(tone: Int, expectedRoot: Note): Boolean = tone % 12 == noteOffset(expectedRoot)

  def cMajorKeyChords: Vector[(Note, ChordType)] =  Vector( (C, Major), (D, Minor), (E, Minor), (F, Major), (G, Major), (A, Minor)    )

  val allNotes: Vector[Note] = Vector(C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, BFlat, B)
  val allMajorMinorChords: Vector[(Note, ChordType)] = for {
    note <- allNotes
    chord <- Vector(Major, Minor)
  } yield (note, chord)

  def rootVoicings(chords: Vector[(Note, ChordType)]): Seq[Description] = chords.map{ case (note, chord) => Description(note, chord, RootPostion)}

  def addAllVoicings(chords: Vector[(Note, ChordType)]): Seq[Description] = {
    for {
      (root, chordType) <- chords
      voicing <- Vector(RootPostion, FirstInversion, SecondInversion)
    } yield Description(root, chordType, voicing)
  }

  def cMajorChordsWithAllVoicings: Seq[Description] = {
    addAllVoicings(cMajorKeyChords)
  }

}

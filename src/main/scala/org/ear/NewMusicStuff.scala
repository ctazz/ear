package org.ear

import javax.sound.midi.MidiChannel


object NewMusicStuff extends App {

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

  sealed trait ChordType
  case object Major extends ChordType
  case object Minor extends ChordType



  case class Description(root: Note, chordType: ChordType, voicing: Voicing)
  //TODO Might get rid of these!
  case class PlayData(rootTone: Int, chordTones: Seq[Int])
  case class Full(description: Description, playData: PlayData)

  //We give the option of lowering the root because otherwise inversions only serve to raise the average pitch
  def makeTriad(theRoot: Int, voicing: Voicing, minor: Boolean = false, lowerOnInversion: Boolean = false): Seq[Int] = {
    val root = if(lowerOnInversion && voicing != RootPostion) theRoot -12 else theRoot
    val third = if(minor) root + 3 else root + 4
    val fifth = root + 7

    voicing match {
      case RootPostion => Vector(root, third, fifth)
      case FirstInversion => Vector(third, fifth, root + 12)
      case SecondInversion => Vector(fifth, root + 12, third + 12)
    }

  }

  def makeTriad(description: Description, rootTone: Int, lowerOnInversion: Boolean): Seq[Int] = {
    makeTriad(rootTone, description.voicing, description.chordType != Major, lowerOnInversion)
  }

  val offsetData: Map[Note, Int] = Map(C -> 0, CSharp -> 1, D -> 2, DSharp -> 3, E -> 4, F -> 5, FSharp -> 6, G -> 7, GSharp -> 8, A -> 9, BFlat -> 10, B -> 11 )
  def offset(note: Note): Int = {
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

  def cMajorChords: Vector[(Note, ChordType)] =  Vector( (C, Major), (D, Minor), (E, Minor), (F, Major), (G, Major), (A, Minor)    )


  def cMajorChordsWithAllVoicings: Seq[Description] = {
    for {
      (root, chordType)  <- Vector( (C, Major), (D, Minor), (E, Minor), (F, Major), (G, Major), (A, Minor)    )
      voicing <- Vector(RootPostion, FirstInversion, SecondInversion)
    } yield Description(root, chordType, voicing)
  }

  def descriptionsByRoot(descs: Seq[Description]): Map[Note, Seq[Description]] =descs.groupBy(_.root)

  println(descriptionsByRoot(cMajorChordsWithAllVoicings))


  assert(makeTriad(60, RootPostion) == Seq(60, 64, 67))
  assert(makeTriad(60, FirstInversion) == Seq(64, 67, 72))
  assert(makeTriad(60, SecondInversion) == Seq(67, 72, 76))
  assert(makeTriad(60, FirstInversion, lowerOnInversion = true) == Seq(52, 55, 60))
  assert(makeTriad(60, SecondInversion, lowerOnInversion = true) == Seq(55, 60, 64))

  assert(makeTriad(60, RootPostion,  minor = true) == Seq(60, 63, 67))
  assert(makeTriad(60, FirstInversion,  minor = true) == Seq(63, 67, 72))
  assert(makeTriad(60, SecondInversion, minor = true) == Seq(67, 72, 75))
  assert(makeTriad(60, FirstInversion, minor = true, lowerOnInversion = true) == Seq(51, 55, 60))
  assert(makeTriad(60, SecondInversion, minor = true, lowerOnInversion = true) == Seq(55, 60, 63))


  implicit val channel: MidiChannel = Player.makeChannels(0)
  cMajorChords.
    map { case (rootNote, chordType) => Description(rootNote, chordType, RootPostion) }.
    //filter(_.chordType == Major).
    foreach { chord =>
      val tone = 60 + offset(chord.root)
      val triadInRootPosition = makeTriad(chord, tone, false)
      println(triadInRootPosition)
      Player.soundNotesForTime(triadInRootPosition, 2000)
    }



}

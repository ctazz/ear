package org.ear




//Should I play interfering non-key chords in between the test chords to make the learning better?
//TODO I should probably have enums for notes, because I don't have any way to show root movement, and I'd like to print out
//the root distinace from one test chord to another
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

  def isCorrectRoot(tone: Int, expectedRoot: Note): Boolean = (tone - 60) % 12 == noteOffset(expectedRoot)

  def cMajorKeyChords: Vector[(Note, ChordType)] =  Vector( (C, Major), (D, Minor), (E, Minor), (F, Major), (G, Major), (A, Minor)    )

  val allNotes: Vector[Note] = Vector(C, CSharp, D, DSharp, E, F, FSharp, G, GSharp, A, BFlat, B)
  val allMajorMinorChords: Vector[(Note, ChordType)] = for {
    note <- allNotes
    chord <- Vector(Major, Minor)
  } yield (note, chord)


  def addAllVoicings(chords: Vector[(Note, ChordType)]): Seq[Description] = {
    for {
      (root, chordType) <- chords
      voicing <- Vector(RootPostion, FirstInversion, SecondInversion)
    } yield Description(root, chordType, voicing)
  }

  def cMajorChordsWithAllVoicings: Seq[Description] = {
    addAllVoicings(cMajorKeyChords)
  }

  def descriptionsByRoot(descs: Seq[Description]): Map[Note, Seq[Description]] =descs.groupBy(_.root)

  def chooseRandomly[A](as: IndexedSeq[A]): A = {
    as(scala.util.Random.nextInt(as.size))
  }

  def chooseIt[A, K](as: Seq[A])(f: A => K): Seq[A] = {
    if(as.size == 1) as
    else {
      val choicesMap: Map[K, Seq[A]] = as.groupBy(f)
      choicesMap(chooseRandomly(choicesMap.keys.toIndexedSeq))
    }
  }

  def chooseRootNote(descs: Seq[Description]): Seq[Description] = {
    chooseIt(descs)(_.root)
  }

  def chooseChordType(descs: Seq[Description]): Seq[Description] = {
    chooseIt(descs)(_.chordType)
  }

  def chooseVoicing(descs: Seq[Description]): Seq[Description] = {
    chooseIt(descs)(_.voicing)
  }

  def choose(descs: Seq[Description]): (Description, Boolean) = {
    val chordsWithVoicing: Seq[Description] = (chooseRootNote _).andThen(chooseChordType).andThen(chooseVoicing).apply(descs)
    if(chordsWithVoicing.size != 1 && chordsWithVoicing.toSet.size != 1) throw new RuntimeException(s"got ${chordsWithVoicing.size} selections: ${chordsWithVoicing}")
    else {
      chordsWithVoicing.head match {
        case theChoice: Description =>
          theChoice.voicing match {
            case RootPostion => (theChoice, false)
            case someInversion => (theChoice, scala.util.Random.nextBoolean  )
          }
      }
    }
  }


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

  assert(isCorrectRoot(62, D))
  assert(isCorrectRoot(74, D))
  assert(!isCorrectRoot(62, DSharp))

  import javax.sound.midi.MidiChannel
  import java.awt.event.KeyListener
  import java.awt.event.{KeyEvent, KeyListener}

  implicit val channel: MidiChannel = Player.makeChannels(0)
  //MidiChannel channel = Player.makeChannelsAsJava().get(0);
  val comparisonNote = 60;
  var offset = 0;


  val keyListener = new KeyListener {
    def keyTyped(e: KeyEvent): Unit = {
      //println(s"key typed is $e")
    }

    def keyPressed(e: KeyEvent): Unit = {
      //println(s"key pressed is $e")
      offset = Keyboard.offset(e.getKeyChar)
      Player.turnOn(comparisonNote + offset, channel)
    }

    def keyReleased(e: KeyEvent): Unit = {
      Player.turnOff(comparisonNote + offset, channel)
    }
  }

  val keyEventDemo = new KeyEventDemo(keyListener)
  keyEventDemo.startIt()


  def chooseAndPlay(choices: Seq[Description], soundingTime: Long, history: Vector[Description] = Vector.empty): Unit = {
    val (desc, makeLowerOnInversion) = choose(choices)
    if(history.lastOption.isDefined && history.head == desc) {
      println(s"skipping repeat. desc was $desc and last chord played was ${history.headOption}" )
      chooseAndPlay(choices, soundingTime, history) //For now not allowing Description repeats, although repeats with different
      //note instantiations might be interestingf
    }else {
      val startingRoot = noteOffset(desc.root) + 60
      val triad = makeTriad(desc, startingRoot, makeLowerOnInversion)
      println(s"chord is $desc and notes played are $triad")
      //println(s"NOT SKIPPING!. desc was $desc and last chord played was ${history.headOption}" )
      Player.soundNotesForTime(triad, soundingTime)
      chooseAndPlay(choices, soundingTime, history :+ desc)
    }
  }

  val primaryCMajorChordsInRootPosition = cMajorKeyChords.collect{case (root, chordType) if chordType == Major => Description(root, chordType, RootPostion) }

  val majorChordsInCMajorKeyWithAllVoicings = addAllVoicings(cMajorKeyChords.filter(_._2 == Major ))
  assert(majorChordsInCMajorKeyWithAllVoicings ==
    Vector(Description(C,Major,RootPostion), Description(C,Major,FirstInversion), Description(C,Major,SecondInversion),
      Description(F,Major,RootPostion), Description(F,Major,FirstInversion), Description(F,Major,SecondInversion),
      Description(G,Major,RootPostion), Description(G,Major,FirstInversion), Description(G,Major,SecondInversion)
    )
  )

  val allMajorChords = addAllVoicings(allMajorMinorChords.filter(_._2 == Major))


  //val allWhiteKeyMajorChords = addAllVoicings(allMajorMinorChords.filter{ case(note, chordType) =>  chordType == Major && whiteKeys.contains(note)   })
  //chooseAndPlay(majorChordsInCMajorKeyWithAllVoicings, 10000 )
  //chooseAndPlay(allMajorChords, 10000 )

  chooseAndPlay(
    addAllVoicings(
      allMajorMinorChords.filter { case (note, chordType) => chordType == Major && whiteKeys.contains(note) }
    ), 5000
  )

/*  chooseAndPlay(
    addAllVoicings(
      allMajorMinorChords.filter { case (note, chordType) => chordType == Major }
    ), 7000
  )*/


/*  cMajorChords.
    map { case (rootNote, chordType) => Description(rootNote, chordType, RootPostion) }.
    //filter(_.chordType == Major).
    foreach { chord =>
      val tone = 60 + noteOffset(chord.root)
      val triadInRootPosition = makeTriad(chord, tone, false)
      println(triadInRootPosition)
      Player.soundNotesForTime(triadInRootPosition, 2000)
    }*/



}

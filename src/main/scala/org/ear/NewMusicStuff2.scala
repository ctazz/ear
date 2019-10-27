package org.ear

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.text.DefaultStyledDocument.ElementSpec

//INSTRUCTIONS!!!
//Just press "D" to go to the next chord
//"A" to hear the chord again

//Should I play interfering non-key chords in between the test chords to make the learning better?
//TODO I should probably have enums for notes, because I don't have any way to show root movement, and I'd like to print out
//the root distinace from one test chord to another
//TODO Play the chord over again after a time if the player doesn't respond
object NewMusicStuff2 extends App {

  //Change from 60 if you want to test CMajor shapes while doing other keys.  For instance, 62 will
  //make D Key sounds and the player can guess the roots using the CMajor shape keyboard
  val cComparisonTone = 60    //Don't Frickin' change this value!
  //Change this value to play the probe chords in different keys
  val comparisonTone = 60

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

  val lowestNoteToPlay = 21
  val lowestC = 24
  val highestNoteToPlay = 108
  //We assume the 0th note is the lowest note
  def expandAcrossKeyboard(smallSeries: Seq[Int]): Seq[Int] = {
    assert(smallSeries.sortWith(_ < _) == smallSeries, "input series of notes is not" +
      s" ordered from lowest to highest. Input series is $smallSeries")

    def lower(seq: Seq[Int], numTimes: Int): Seq[Int] = {
      seq.map(note => note - (12 * numTimes))
    }

    def lowerUntil(note: Int, numTimesLowered: Int): (Int, Int) = {
      println(s"inside lowerUntil, args are ($note, $numTimesLowered)")
      if (note < lowestNoteToPlay) ( note + 12, numTimesLowered - 1)
      else if (note == lowestNoteToPlay) (note, numTimesLowered)
      else lowerUntil(note - 12, numTimesLowered + 1)
    }

    def raiseAndExpand(lowered: Seq[Int]): Seq[Int] = {

      def addMore(addedSoFar: Seq[Int], numTimes: Int): Seq[Int] = {
        println(s"addedSoFar is $addedSoFar")
        val newlyRaised = lowered.map(note => note + (12 * (numTimes + 1)  )).filter(_ <= highestNoteToPlay)
        if(newlyRaised.size < lowered.size) lowered ++ addedSoFar ++ newlyRaised
        else addMore( addedSoFar ++ newlyRaised, numTimes + 1  )

      }

      addMore(Seq.empty, 0)

    }

    val loweredSeries = smallSeries.headOption match {
      case None => Seq.empty
      case Some(h) =>
        println(s"h is $h")
        val numTimesToLower = lowerUntil(h, 0)._2
        println(s"numTimesToLower = $numTimesToLower")
        lower(smallSeries, numTimesToLower)
    }

    println(s"lowered series is $loweredSeries")
    raiseAndExpand(loweredSeries)

  }



  import Music._
  //TODO comparison tone machinations getting pretty ugly
  def addLowRootTone(dAndA: DescriptonAndActual): Seq[Int] = {

    //+12 so the lowest notes aren't so damn low
    val lowestRootToneThatWillFitOnTheKeyboard: Int =
      noteOffset(dAndA.desc.root) + lowestC + 12 + comparisonTone - cComparisonTone match {
        case x if x < lowestC => x + 12
        case x => x
      }

    //val lowestRootToneThatWillFitOnTheKeyboard = noteOffset(dAndA.desc.root) + lowestC
    val tonesThatMightHaveHadRootAdded: Seq[Int] = dAndA.actual.headOption.map(lowestToneCurrentlyBeingPlayed =>
      if(lowestToneCurrentlyBeingPlayed < lowestRootToneThatWillFitOnTheKeyboard){
        println(s"TODO. Lowest note is below the lowest allowable root note. Maybe we should throw" +
          s" an exception or, delete the lowest tone.  Not sure???")
        dAndA.actual
      }
      else lowestRootToneThatWillFitOnTheKeyboard +: dAndA.actual
    ).getOrElse(
      //TODO Let's have Vectors everywhere and not Seqs
      Vector(lowestRootToneThatWillFitOnTheKeyboard)
    )

    println(s"lowest note now being played for chord ${dAndA.desc} with tones ${dAndA.actual} is ${tonesThatMightHaveHadRootAdded.head}")
    tonesThatMightHaveHadRootAdded
  }


  def descriptionsByRoot(descs: Seq[Description]): Map[Note, Seq[Description]] = descs.groupBy(_.root)

  import Choosing._

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

  assert(
    expandAcrossKeyboard(Seq(60, 64, 67)) == Seq(24, 28, 31, 36, 40, 43, 48, 52, 55, 60, 64, 67, 72, 76, 79, 84, 88, 91, 96, 100, 103, 108),
    s"actual result is ${expandAcrossKeyboard(Seq(60, 64, 67))}"
  )
  assert(
    expandAcrossKeyboard(Vector(69, 72, 76)) ==
      Seq(21, 24, 28, 33, 36, 40, 45, 48, 52, 57, 60, 64, 69, 72, 76, 81, 84, 88, 93, 96, 100, 105, 108),
    s"actual result is ${expandAcrossKeyboard(Seq(69, 72, 76))}"
  )

  assert(isCorrectRoot(-3, A))
  assert(isCorrectRoot(9, A))
  assert(isCorrectRoot(67, G))
  assert(isCorrectRoot(55, G))
  assert(isCorrectRoot(62, D))
  assert(isCorrectRoot(74, D))
  assert(!isCorrectRoot(62, DSharp))

  import javax.sound.midi.MidiChannel
  import java.awt.event.KeyListener
  import java.awt.event.{KeyEvent, KeyListener}

  val synth = Player.createSynth
  val playerChannel = Player.channelAndInstrument(synth, 1, 120)
  //See https://en.wikipedia.org/wiki/General_MIDI Program Change Events for the sounds
  // corresponding to the instrurment number
  val testerChannel: MidiChannel = Player.channelAndInstrument(synth, 0, 30)
  //Player.channelAndInstrument(synth, 0, 69)
    //Player.channelAndInstrument(synth, 0, 30)//Player.channelAndInstrument(synth, 0, 41)    // Player.makeChannels(0)


  var offsetForPlayerKeyboard = 0;
  val currRoot: AtomicReference[Option[Note]] = new AtomicReference(None)
  val doDelay = new AtomicBoolean(false)
  val playerChoseCorrectRoot = new AtomicBoolean(false)
  val playerCommand = new AtomicReference[String]("")


  val keyListener = new KeyListener {
    def keyTyped(e: KeyEvent): Unit = {
      //println(s"key typed is $e")
    }

    def keyPressed(e: KeyEvent): Unit = {
      //println(s"key pressed is $e")
      Keyboard.offset(e.getKeyChar).map { offset =>
        Player.turnOn(comparisonTone + offset, playerChannel)
        currRoot.get.foreach { currentRoot =>
          //I used to have this as isCorrectRoot(comparisonTone + offset, currentRoot)
          //But removing comparisionTone lets the player use the CMajor keyboard to guess chords in any key, and
          //we vary the key simply by changing the comparison tone.
          //TODO This is a bit confusing, isn't it?
          val correctRoot = isCorrectRoot(offset, currentRoot)
          println(s"player's's note was ${comparisonTone + offset} and currentRoot is $currentRoot")
          println("correct root? " + correctRoot)
          if(correctRoot) {
            playerChoseCorrectRoot.set(true)
            println("trying to turn off the player's's note")
            //TODO!!!  We should probably set the offsetForPlayerKeyboard above, as soon as we get the offset arg in this map function, and base everything off of that.
            Player.turnOff(comparisonTone + offsetForPlayerKeyboard, playerChannel)
          }

        }
      }.getOrElse {
        println("not a valid note. Might be a player command")
        //doDelay.set(true)
        playerCommand.set(e.getKeyChar.toString())

      }

    }

    def keyReleased(e: KeyEvent): Unit = {
      println("key released")
      Player.turnOff(comparisonTone + offsetForPlayerKeyboard, playerChannel)
    }
  }

  val keyEventDemo = new KeyEventDemo(keyListener)
  keyEventDemo.startIt()

  def playback(history: Vector[DescriptonAndActual], soundingTime: Long, numOfPreviousProbesToPlay: Int): Unit = {
    println(s"numOfPreviousProbesToPlay is $numOfPreviousProbesToPlay")
    val allProbesToPlay = history.slice(history.size - numOfPreviousProbesToPlay - 1, history.size)
    allProbesToPlay.foreach(x => println(x.desc))
    allProbesToPlay.foreach{probe =>
      Player.soundNotesForTime(probe.actual, soundingTime)(testerChannel)
    }
  }

  //Takng out a lot of functionality that waitForCorrectRootOrPlayerToAskForNext handled, but it did that
  //work for one chord at a time
  def waitForPlayerResponse(newChords: Vector[DescriptonAndActual], soundingTime: Long): Unit = {
    {
      val command = playerCommand.get()
      playerCommand.set("")
      command
    } match {
      case "D" =>
        println("===> player going to next chord or series of chords")
        return
      case "A" =>
        println(s"===> player wants to hear chord again. ${newChords.last.desc}")
        playChords(newChords, soundingTime)
        waitForPlayerResponse(newChords, soundingTime)

      case other =>
        Thread.sleep(200)
        waitForPlayerResponse(newChords, soundingTime)
    }
  }

  //===============================================================================================
  //Important. No longer using this function, but need to pull some functionality from it that's
  //not yet implemented in the new function.
  //===============================================================================================
  //TODO We could probably use timer here, and call the chooseAndPlay function when we're done
  //TODO This was written under the assumption that players will only be reacting to one chord at a time.
  //I should make this better, but instead, for now I'm writing a more simple function that only responds to
  //two commands: 1) play those chords again   2) on to the next sequence of chords
  //and then cedes control to the calling function.
  //I should come back to this and use it to write a function
  //that also lets the player type in the chord routes as a test. For right now the only player, me, is using a different
  //keyboard to find out if his/her/my guesses were correct.
  //This code also can play the root chord when asked, or regenerate chords that have the root in a low base note,
  //and I think I should make at least the base root bit work again.
  def waitForCorrectRootOrPlayerToAskForNext(history: Vector[DescriptonAndActual], soundingTime: Long): Unit = {
    if(playerChoseCorrectRoot.get){
      println(s"player got correct root")
      playerChoseCorrectRoot.set(false)
      Thread.sleep(1000)
      return
    }
    else {

      //TODO Problem. We could be removing from playerCommand while player is adding to it!
      //This would be even greater trouble if we wanted to allow player to do something like P3 for play the
      //previous 3 chords, since P3 is a two character command.
      {
        val command = playerCommand.get()
        playerCommand.set("")
        command
      } match {
        case "D" =>
          println("going to next chord")
          return
        case "A" =>
          println(s"player wants to hear chord again. ${history.last.desc}")
          Player.soundNotesForTime(history.last.actual, soundingTime)(testerChannel)
          waitForCorrectRootOrPlayerToAskForNext(history, soundingTime)

        case "R" =>
          println(s"playing root chord")
          Player.soundNotesForTime(history.head.actual, soundingTime)(testerChannel)
          println(s"playing latest chord")
          Player.soundNotesForTime(history.last.actual, soundingTime)(testerChannel)
          waitForCorrectRootOrPlayerToAskForNext(history, soundingTime)
        case "b" => {
          println("playing chord that may have had low root note added")
          val sequenceOfNotesThatMightHaveHadLowRootAdded = addLowRootTone(history.last)
          println("playing root of this chord: " + history.last.desc)
          Player.soundNotesForTime(sequenceOfNotesThatMightHaveHadLowRootAdded, soundingTime)(testerChannel)
          waitForCorrectRootOrPlayerToAskForNext(history, soundingTime)
        }
        case x if !x.isEmpty && x.forall(_.isDigit) =>
          playback(history, soundingTime, x.toInt)
          waitForCorrectRootOrPlayerToAskForNext(history, soundingTime)
        case other =>
          Thread.sleep(200)
          waitForCorrectRootOrPlayerToAskForNext(history, soundingTime)
      }

    }

  }

  def makeTriadBasedOnComparisonNote(desc: Description, makeLowerOnInversion: Boolean): Seq[Int] = {
    val startingRoot = noteOffset(desc.root) + comparisonTone
    makeTriad(desc, startingRoot, makeLowerOnInversion)
  }

  def playChords(newChords: Seq[DescriptonAndActual], soundingTime: Long) = {
    newChords.map { descriptionAndActual =>
      println(s"playing $descriptionAndActual")
      Player.soundNotesForTime(descriptionAndActual.actual, soundingTime)(testerChannel)
    }
  }

  //TODO. If I'm trying to sing these, we should see what happens if I narrow the range over which we can choose notes.
  def chooseAndPlay(choices: Seq[Description], soundingTime: Long, numToChoose: Int = 1,
                    history: Vector[DescriptonAndActual] = Vector.empty): Unit = {
    println(s"entering chooseAndPlay. previous chord was ${history.lastOption}")

    def loop(newChords: Vector[DescriptonAndActual]): Vector[DescriptonAndActual] = {
      println(s"newChords length is ${newChords.length} and numToChoose = $numToChoose")
      if(newChords.length == numToChoose)
        newChords
      else {
        val (desc, makeLowerOnInversion) = choose(choices)
        //If we already have some new chords and the last one has the same description as the one we just chose,
        //throw the one we just chose away and try again.
        //Also throw away and try again if we don't have any new chords and the last chord in our history has a description that
        //matches the description we just chose.
        if((!newChords.isEmpty && (newChords.lastOption.map(_.desc)) == Some(desc)) || history.lastOption.map(_.desc) == Some(desc) ) {
          println(s"skipping new chord $desc because it matches our last one")
          loop(newChords)
        }
        else {
          println(s"thread is ${Thread.currentThread().getName}. New chord description is $desc")
          val triad = //expandAcrossKeyboard(
            makeTriadBasedOnComparisonNote(desc, makeLowerOnInversion)
          //)
          val descriptonAndActual = DescriptonAndActual(desc, triad)
          loop(newChords :+ descriptonAndActual)
        }
      }

    }

    //This play won't work with multiple chords, mainly because the waitForCorrectRootOrPlayerToAskForNext it calls won't work for that.
/*    def play(triad: Seq[Int], desc: Description, newHistory: Vector[DescriptonAndActual]): Unit = {

      //Where do we put this? Well, not doing player choose for now!
      currRoot.set(Some(desc.root))
      //println(s"NOT SKIPPING!. desc was $desc and last chord played was ${history.headOption}" )

      //This playing has to sound in a loop now!
      println(s"notes played are $triad and chord is $desc")
      //This ...
      //val sequenceOfNotesThatMightHaveHadLowRootAdded = addLowRootTone(descriptonAndActual)
      //Player.soundNotesForTime(sequenceOfNotesThatMightHaveHadLowRootAdded, soundingTime)(testerChannel)
      //Or this ...
      Player.soundNotesForTime(triad, soundingTime)(testerChannel)

      //Really just waiting for player to go on to the next one now!
      waitForCorrectRootOrPlayerToAskForNext(newHistory, soundingTime)
      chooseAndPlay(choices, soundingTime, numToChoose, newHistory)
    }*/
    //TODO Probably don't need doDelay anymore
    if(doDelay.get()){
      doDelay.set(false)
      println("delaying")
      Thread.sleep(3000)
      chooseAndPlay(choices, soundingTime, numToChoose, history)
    }else {

      val newChords = loop(Vector.empty)
      val newHistory = history ++ newChords
      playChords(newChords, soundingTime)
      waitForPlayerResponse(newChords, soundingTime)
      chooseAndPlay(choices, soundingTime, numToChoose, newHistory)


      //Old stuff
/*      val (desc, makeLowerOnInversion) = choose(choices)
      if (history.lastOption.map(_.desc) == Some(desc)) {
        println(s"skipping repeat. desc was $desc and last chord played was ${history.headOption}")
        chooseAndPlay(choices, soundingTime, numToChoose, history) //For now not allowing Description repeats, although repeats with different
        //note instantiations might be interestingf
      } else {
        println(s"thread is ${Thread.currentThread().getName}. New chord description is $desc and previous was ${history.lastOption}")
        val triad = //expandAcrossKeyboard(
          makeTriadBasedOnComparisonNote(desc, makeLowerOnInversion)
        //)
        val descriptonAndActual = DescriptonAndActual(desc, triad)
        val newHistory = history :+ descriptonAndActual

        play(triad, desc, newHistory)


      }*/


    }
  }

  val primaryCMajorChordsInRootPosition = cMajorKeyChords.collect{case (root, chordType) if chordType == Major => Description(root, chordType, RootPostion) }

  val majorChordsInCMajorKeyWithAllVoicings = addAllVoicings(cMajorKeyChords.filter(_._2 == Major ))
/*  assert(majorChordsInCMajorKeyWithAllVoicings ==
    Vector(Description(C,Major,RootPostion), Description(C,Major,FirstInversion), Description(C,Major,SecondInversion),
      Description(F,Major,RootPostion), Description(F,Major,FirstInversion), Description(F,Major,SecondInversion),
      Description(G,Major,RootPostion), Description(G,Major,FirstInversion), Description(G,Major,SecondInversion)
    )
  )*/

  val allMajorChords = addAllVoicings(allMajorMinorChords.filter(_._2 == Major))


  //val allWhiteKeyMajorChords = addAllVoicings(allMajorMinorChords.filter{ case(note, chordType) =>  chordType == Major && whiteKeys.contains(note)   })
  //chooseAndPlay(majorChordsInCMajorKeyWithAllVoicings, 10000 )
  //chooseAndPlay(allMajorChords, 10000 )

  /*  chooseAndPlay(
      addAllVoicings(
        allMajorMinorChords.filter { case (note, chordType) => chordType == Major && whiteKeys.contains(note) }
      ), 5000
    )*/

  /*
    chooseAndPlay(
      addAllVoicings(
        //allMajorMinorChords
        allMajorMinorChords.filter { case (note, chordType) => chordType == Minor }
      ).filter(_.voicing == RootPostion), 5000 //This parameter makes a huge difference in my accuracy. Sounding the chord past the sound of my player's response makes for much better accuracy
    )
  */



  val starter: DescriptonAndActual = Description(C, Major, RootPostion) match {
    case desc => DescriptonAndActual(desc,
      //expandAcrossKeyboard(
      makeTriadBasedOnComparisonNote(desc, false)
      //)
    )
  }

  /*  val chordsAndVoicingsToPlay =  rootVoicings(
      //allMajorMinorChords
      cMajorKeyChords ++ Vector(BFlat -> Major, DSharp -> Major, D -> Major)
    )*/
  //TODO Temporarily not doing first inversion! And not doing assertion on majorChordsInCMajorKeyWithAllVoicings
  val chordsAndVoicingsToPlay =  addAllVoicings(
    //allMajorMinorChords
    //popChordsInCMajor
    cMajorKeyChords
  )//.filter(_.chordType == Major)

  Player.soundNotesForTime(starter.actual, 4000)(testerChannel)
  //Player.soundNotesForTime(triad, 5000)(testerChannel)
  //Was 6000 with just one chord
  chooseAndPlay(
    chordsAndVoicingsToPlay, 2000 //This parameter makes a huge difference in my accuracy. Sounding the chord past the sound of my player's response makes for much better accuracy,
    , 3, history = Vector(starter),
  )



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


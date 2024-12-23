package org.ear

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

//Based on NewMusicStuff
/*
This is the one I've been using most recently (2022)
It's good at playing chords one at a time, not good at playing chords in succession.
 */

//Should I play interfering non-key chords in between the test chords to make the learning better?
//TODO I should probably have enums for notes, because I don't have any way to show root movement, and I'd like to print out
//the root distinace from one test chord to another
//TODO Play the chord over again after a time if the user doesn't respond
object MulitChords extends App {


  val cComparisonTone = 60    //Don't Frickin' change this value!

  //IMPORTANT SETTING
  //Change from 60 if you want to test CMajor shapes while doing other keys.  For instance, 62 will
  //make D Key sounds and the player can guess the roots using the CMajor shape keyboard
  //Change this value to play the probe chords in different keys
  //But the chords will print out as though you're in C!!!!!
  //55 is G, 65 is F, 57 is A, 62 is D, 64 is E
  val comparisonTone = 60;//C is 60  //<===Moving down 12 makes all notes lower by an octave without changing the key
  //Currently I find switching from G to D hard

  //important setting
  val makeLowestNoteInTriadLower = false
  //important setting
  //If you have very limited choices, you might want to allow exact repeats.
  val preventRepeats = true

  def soundTestNotes(notes: Seq[Int], soundingTime: Long) = {
    val notesToPlay = if(makeLowestNoteInTriadLower) addLowTone(notes) else notes
    if(makeLowestNoteInTriadLower) println(s"Lowest note is ${notesToPlay.head}")
    Player.soundNotesForTime(notesToPlay, soundingTime)(testerChannel)
  }

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
  def addLowTone(notes: Seq[Int]): Seq[Int] = {
    val muchLower = notes.head - 2 * 12
    val lowNoteToUse = if(muchLower <= 30) muchLower + 12 else muchLower
    lowNoteToUse +: notes
  }

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
    //descs.filter(_.voicing == RootPostion)
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
            case _ => (theChoice, scala.util.Random.nextBoolean  )
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

  import java.awt.event.{KeyEvent, KeyListener}
  import javax.sound.midi.MidiChannel

  val synth = Player.createSynth
  val playerChannel = Player.channelAndInstrument(synth, 1, 120) //used to be instrument 8

  //important setting
  //This is a good instrument for the testerChannel. It sounds like voices:
  //Player.channelAndInstrument(synth, 0, 54)
  val testerChannel: MidiChannel = Player.channelAndInstrument(synth, 0, 54)
  //Player.channelAndInstrument(synth, 0, 35) //pretty, does not sustain long
  //Player.channelAndInstrument(synth, 0, 30) //relatively loud, has long sustain
  //instrumentNum was 35  // Player.makeChannels(0)


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
          //But removing comparisionTone lets the user use the CMajor keyboard to guess chords in any key, and
          //we vary the key simply by changing the comparison tone.
          //TODO This is a bit confusing, isn't it?
          val correctRoot = isCorrectRoot(offset, currentRoot)
          println(s"user's note was ${comparisonTone + offset} and currentRoot is $currentRoot")
          println("correct root? " + correctRoot)
          if(correctRoot) {
            playerChoseCorrectRoot.set(true)
            println("trying to turn off the user's note")
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



  def playback(history: Vector[DescriptonAndActual], soundingTime: Long, numOfPreviousProbesToPlay: Int): Unit = {
    println(s"numOfPreviousProbesToPlay is $numOfPreviousProbesToPlay")
    val allProbesToPlay = history.slice(history.size - numOfPreviousProbesToPlay - 1, history.size)
    allProbesToPlay.foreach(x => println(x.desc))
    allProbesToPlay.foreach{probe =>
      soundTestNotes(probe.actual, soundingTime)
    }
  }

  //TODO We could probably use timer here, and call the chooseAndPlay function when we're done
  def waitForCorrectRoot(history: Vector[DescriptonAndActual], soundingTime: Long): Unit = {
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
        case "A" =>
          println("player wants to hear chord again")
          Player.soundNotesForTime(history.last.actual, soundingTime)(testerChannel)
          waitForCorrectRoot(history, soundingTime)

        case "R" =>
          println(s"playing root chord")
          Player.soundNotesForTime(history.head.actual, soundingTime)(testerChannel)
          println(s"playing latest chord")
          Player.soundNotesForTime(history.last.actual, soundingTime)(testerChannel)
          waitForCorrectRoot(history, soundingTime)
        case "b" => {
          println("playing chord that may have had low root note added")
          val sequenceOfNotesThatMightHaveHadLowRootAdded = addLowRootTone(history.last)
          println("playing root of this chord: " + history.last.desc)
          Player.soundNotesForTime(sequenceOfNotesThatMightHaveHadLowRootAdded, soundingTime)(testerChannel)
          waitForCorrectRoot(history, soundingTime)
        }
        case "n" => {
          //n stands for next. Didn't get this one write or wrong, go to the next one. (We could decide to mark this wrong if we're keeping score.)
          playerChoseCorrectRoot.set(false)
          Thread.sleep(1000)
        }

        case x if !x.isEmpty && x.forall(_.isDigit) =>
          playback(history, soundingTime, x.toInt)
          //TODO CHUCK TEMP. usually use 'soundingTime', but 3000 gives me chance to sing the chords
          waitForCorrectRoot(history, soundingTime)
        case other =>
          Thread.sleep(200)
          waitForCorrectRoot(history, soundingTime)
      }

    }

  }

  def makeTriadBasedOnComparisonNote(desc: Description, makeLowerOnInversion: Boolean): Seq[Int] = {
    val startingRoot = noteOffset(desc.root) + comparisonTone
     makeTriad(desc, startingRoot, makeLowerOnInversion)
  }


  //=============BEGIN NEW STUFF =================
  val lowestTriadTone = 36
  val highestTriadTone = 96


  def addDescriptionAndActuals( choices: Seq[Description], history: Vector[DescriptonAndActual] , numLeftToCreate: Int,
                                acc: Vector[DescriptonAndActual] = Vector.empty):
  Vector[DescriptonAndActual] = {
    //TODO We add to history internally just so we don't make the same description twice. If ccalling code also keeps history, that's a
    //bit of a waste
    if(numLeftToCreate== 0) acc
    else {
      val (desc, makeLowerOnInversion) = choose(choices)
      if (history.lastOption.map(_.desc) == Some(desc) && preventRepeats) {
        println(s"skipping repeat. desc was $desc and last chord played was ${history.headOption}")
        addDescriptionAndActuals( choices,  history, numLeftToCreate, acc) //For now not allowing Description repeats, although repeats with different
        //note instantiations might be interestingf
      }
      else {
        //TODO Could expandAcrossKeyborad
        val triad = //expandAcrossKeyboard(
          makeTriadBasedOnComparisonNote(desc, makeLowerOnInversion)
        //)
        if(triad.head < lowestTriadTone || triad.last > highestTriadTone)  addDescriptionAndActuals(choices, history, numLeftToCreate, acc)
        else {
          val descriptonAndActual = DescriptonAndActual(desc, triad)
          val newHistory = history :+ descriptonAndActual
          addDescriptionAndActuals(choices, newHistory, numLeftToCreate - 1, acc :+ descriptonAndActual)
        }
      }
    }

  }

  def playLatestSequence(latestSequence: Vector[DescriptonAndActual], soundingTime: Long): Unit = {
    latestSequence.foreach{x =>
      println(s"playing $x")
      soundTestNotes(x.actual, soundingTime)
    }
  }
  //=============END NEW STUFF =================

  def chooseAndPlay(choices: Seq[Description], soundingTime: Long, history: Vector[DescriptonAndActual] = Vector.empty): Unit = {
    println(s"entering chooseAndPlay. previous chord was ${history.lastOption}")
    //TODO Probably don't need doDelay anymore
    if(doDelay.get()){
      doDelay.set(false)
      println("delaying")
      Thread.sleep(3000)
      chooseAndPlay(choices, soundingTime, history)
    }else {
      val (desc, makeLowerOnInversion) = choose(choices)
      if (history.lastOption.map(_.desc) == Some(desc)) {
        println(s"skipping repeat. desc was $desc and last chord played was ${history.headOption}")
        chooseAndPlay(choices, soundingTime, history) //For now not allowing Description repeats, although repeats with different
        //note instantiations might be interestingf
      } else {
        println(s"thread is ${Thread.currentThread().getName}. New chord description is $desc and previous was ${history.lastOption}")
        val triad = //expandAcrossKeyboard(
          makeTriadBasedOnComparisonNote(desc, makeLowerOnInversion)
        //)
        val descriptonAndActual = DescriptonAndActual(desc, triad)
        val newHistory = history :+ descriptonAndActual
        println(s"chord is $desc and notes played are $triad")
        currRoot.set(Some(desc.root))
        //println(s"NOT SKIPPING!. desc was $desc and last chord played was ${history.headOption}" )

        //This ...
        //val sequenceOfNotesThatMightHaveHadLowRootAdded = addLowRootTone(descriptonAndActual)
        //Player.soundNotesForTime(sequenceOfNotesThatMightHaveHadLowRootAdded, soundingTime)(testerChannel)
        //Or this ...
        Player.soundNotesForTime(triad, soundingTime)(testerChannel)

        waitForCorrectRoot(newHistory, soundingTime)
        chooseAndPlay(choices, soundingTime, newHistory)
      }
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
/*  val chordsAndVoicingsToPlay =  addAllVoicings(
    allMajorMinorChords
    //cMajorKeyChords
  )//.filter(_.chordType == Major)*/
  //CHUCK CHUCK XXXXMade chooseVoicing function choose only Descriptions that include SecondInversion Voicing, Oct 14,2022
  //XXXXNOW choosing only the Major chords in C Major, immediately below!
  //Changed testerChannel from 41 to 35
  //CHUCK CHUCK HERE HERE Changed to ccMajorChords on Oct 13, 2022
  //xxxCHANGED comparisonTone to 62 from 60, so we're using the CMajor shape in the key of D
  val chordAndVoicingChoices: Seq[Description] =  addAllVoicings(
  //cMajorKeyChords
  cMajorKeyChords ++ Vector((BFlat, Major))
    //ncMajorKeyChords ++ Vector((BFlat, Major))    ++ Vector( (D, Major ),  (BFlat, Major), (E, Major), (F, Minor))
    //aMinorChords
  //cMajorKeyChords.filter(tup => tup._2 == Major || tup._1 == A)
    //cMajorKeyChords.filter(tup => tup._2 == Minor  )
   //cMajorKeyChords.filter(_._2 == Major)
  //cMajorKeyChords.filter(tup => tup._2 == Major)
        //allMajorMinorChords
  )//.filter( x => x.voicing == SecondInversion )
  //.filter(x => (x.voicing == SecondInversion || x.voicing == FirstInversion) && x.chordType == Minor)


  //Good for practice singing chord intervals
  val allHaveAInRoot = Vector(
  Description(D, Minor, SecondInversion),  //in key
    Description(D, Major, SecondInversion), //out of key
  Description(A, Minor, RootPostion),  //in key
    Description(A, Major, RootPostion), //out of key
  Description(F, Major, FirstInversion), //in key
    Description(FSharp, Minor, FirstInversion)
  )


  val keyEventDemo = new KeyEventDemo(keyListener)
  keyEventDemo.startIt()
  //TODO Maintain history here
  //val newOnes = addDescriptionAndActuals(chordAndVoicingChoices, Vector.empty, 2)
  //playLatestSequence(newOnes, soundingTime = 3000)

  def loop(hist: Vector[DescriptonAndActual], howManyInSequence: Int, soundingTime: Long, choices: Seq[Description]): Unit = {
    val newOnes = addDescriptionAndActuals(choices, hist, howManyInSequence)
    val newHistory = hist ++ newOnes
    playLatestSequence(newOnes, soundingTime = soundingTime)
    //This is still the old version of waitForCorrectRoot, which now hasn't been given the correct root.
    //For now all we care about is the ability to go to the next sequence, and to play the correct number of chords back if the
    //player presses a digit key.
    //We would like, in the future, to allow the player to guess the correct roots by using the keyboard.
    //And more importantly, have the be able to ask for latest sequence of chords to be played with
    //the root played low.
    waitForCorrectRoot(newHistory, soundingTime)
    loop(newHistory, howManyInSequence, soundingTime, choices)
  }

  //Usually I use chordAndVoicingChoices here
  loop(Vector.empty, 1 , 2000, chordAndVoicingChoices)
  //testerChannel (definition occurs earlier in this program) and soundingTime make a big different.
  //With testerChanel 30 I can hear the top note and thus kow the inversion
  //!!!highestNoteToPlay doesn't seem to affect how high the chord tones go!!
 //Instead, lowestTriadTone and highestTriadTone, which are used in addDescriptionAndActuals, set the range for chord tones
  //TODO Pass the range into addDescreptionAndActuals!

}

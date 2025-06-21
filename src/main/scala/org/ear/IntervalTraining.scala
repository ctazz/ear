package org.ear

import java.util.concurrent.atomic.AtomicReference

import org.ear.NewMusicStuff.keyListener
//A major sixth descendenig Over There-. O-Ver

object IntervalTraining extends App {

  //sets what key you're in. 60 is C
  val comparisonTone = 65
  //Important. Besides lebal offsets, also see legalIntervals!
  //These legal offsets restrict the game tones to diatonic in the major key.
  val legalOffsets = Vector(-3,-1,0, 2, 4,5,7,9,11,12,14,16,17)
  //For minor scale
  //val legalOffsets = Vector(-4, -2, 0, 2, 3,5,7,8,10,12,14,15,17)
  //For harmonic minor
  //val legalOffsets = Vector(-4, -1, 0, 2, 3,5,7,8,11,12,14,15,17)
  //For mixolydian
  //val legalOffsets = Vector(-3,-2,0, 2, 4,5,7,9,10,12,14,16,17)
  //Major plus flat 7
  //val legalOffsets = Vector(-3,-2,-1, 0, 2, 4,5,7,9,10, 11, 12,14,16,17)
  //val legalOffsets = (-12 to 12).toVector

  import javax.sound.midi.MidiChannel
  import java.awt.event.KeyListener
  import java.awt.event.{KeyEvent, KeyListener}

  val synth = Player.createSynth
  val playerChannel =   Player.channelAndInstrument(synth, 0, 120) //q Player.channelAndInstrument(synth, 0, 35) //Player.channelAndInstrument(synth, 1, 120)
  val testerChannel: MidiChannel = //Player.channelAndInstrument(synth, 0, 0)
  Player.channelAndInstrument(synth, 0, 54) //This one sounds like a human voice
    //Player.channelAndInstrument(synth, 0, 120)
  //Player.channelAndInstrument(synth, 0, 35)
    //Player.channelAndInstrument(synth, 0, 28) //17 and 28 are interesting too    // Player.makeChannels(0)

  val droneChannel = Player.channelAndInstrument(synth, 0, 54)
  Player.soundNotesForTimeAsync(Seq(comparisonTone -24), 1000000000L)(droneChannel)

  //An offset is a difference from the 0 note
  //The intervals tell us the difference between each note and its preceding noe
  //The offsets tell us which actual note is being played (although we need to add some value to each offset to actually sound the a note)
  def chooseNextIntervalAndOffset(legalIntervals: Vector[Int], currentOffset: Int, lowestOffset: Int, highestOffset: Int): (Int, Int) = {
    val diff = Choosing.chooseRandomly(legalIntervals)
    val nextOffset = diff + currentOffset

    //TODO THIS legalOffsets.containst stuff is a quick shortcut to restrict the set of notes (not intervals, notes) that we choose from
    //If you don't restrict by legalOffssets then you're including all intervals within lowestOffset and highestOffset
    if(nextOffset < lowestOffset || nextOffset > highestOffset || !legalOffsets.contains(nextOffset)) {
      chooseNextIntervalAndOffset(legalIntervals, currentOffset, lowestOffset, highestOffset)
    }
    else {
      (diff, nextOffset)
    }
  }

  val nextIntervalFunc: (Vector[Int], Int) => Tuple2[Int, Int] = (legalIntervals, currentOffset) =>
    chooseNextIntervalAndOffset(legalIntervals, currentOffset,
      Keyboard.low,
      Keyboard.high)

  def takeN(n: Int, currentOffset: Int, legalIntervals: Vector[Int]): Vector[(Int, Int)] = {
    //(0 until n).foldLeft( Vector.empty[Int]  )( (acc, index) => acc :+   )

    def loop(accum: Vector[(Int, Int)], currentOffset: Int): Vector[(Int, Int)] = {
      if(accum.size >= n) accum
      else
        nextIntervalFunc(legalIntervals, currentOffset) match {
          case (diff, nextOffset) =>
            loop(accum :+ (diff, nextOffset), nextOffset)
        }
    }

    loop(Vector.empty, currentOffset)

  }

  //IMPORTANT Setting:
  //legalIntervals Limits the distance between note n and note n+1
  //val legalIntervals: Vector[Int]= (-5 to 5).toVector.filterNot(_ == 0)
  //val legalIntervals: Vector[Int]= (-12 to 12).toVector.filterNot(_ == 0)
  val legalIntervals: Vector[Int]= (-7 to 7).toVector.filterNot(_ == 0)
  //val legalIntervals: Vector[Int]= Vector(2, -2, 3,-3,4,-4, 5, -5)
  //val legalIntervals: Vector[Int]= Vector(1, -1, 2, -2, 5, -5, 6, -6, 7, -7)
  //val legalIntervals: Vector[Int]= (-9 to 9).toVector.filterNot(x => math.abs(x) < 5)
  //Currently numTestIntervalsToPlay = 3 and timeToSoundTestNote = 7 is easy for major scale
  val numTestIntervalsToPlay = 3
  //timeToSoundTestNote = 2000 makes it almost interval by interval. 1000 makes it clearly a sequence, although a slow sequence
  //I'm ok with 3 and 1500, and maybe even 3 and 1000, with harmonic minor
  val timeToSoundTestNote =   1000 //2000//500 //1000 //600 //1000 2000 //2000 4 at a time is good
  val timeToWaitAfterCorrestResponse = timeToSoundTestNote //300 //500

  //TODO Should these be atomic? Pretty sure they should, at least the ones that are accessed both by sound generation and code that responds to player keystrokes
  var offsetForPlayerKeyboard = 0
  var previousOffsetForTestKeyboard = 0
  var mostRecentOffsetForTestKeyboard = 0

  var currentIntervalSequence = new AtomicReference(Vector.empty[Int])
  var playerIntervalSequence = new AtomicReference(Vector.empty[Int])

  var currentSequence = new AtomicReference(Vector.empty[Int])
  var playersSequence = new AtomicReference(Vector.empty[Int])

  def chooseAndSound(currentOffset: Int, numToTake: Int, timeToSound: Long): Unit = {
    //TODO Make this sound the current offfset too, and don't sound the current offset before this.
    //That is, make this like what we do when replaying the sound in response to the user's "A" (again)
    //And make sure this works well with both interval and actual-note testing
    val intervalsAndOffsets: Vector[(Int, Int)] = takeN(numToTake, currentOffset, legalIntervals)
    println("internvalsAndOffsets=" + intervalsAndOffsets)

    previousOffsetForTestKeyboard = currentOffset
    currentSequence.set(intervalsAndOffsets.map(_._2))
    currentIntervalSequence.set(intervalsAndOffsets.map(_._1))

    mostRecentOffsetForTestKeyboard = intervalsAndOffsets.last._2

    intervalsAndOffsets.foreach { case (interval, newOffset) =>
      println(s"@@the diff is $interval and the new offset is $newOffset")
      println(s"@@note is " + Music.classifyTheNote(comparisonTone + newOffset))
      Player.soundNotesForTime(Seq(comparisonTone + newOffset), timeToSound)(testerChannel)
    }
  }

  def isDigit(ch: Char): Boolean = {
    val asciiNum = ch.toInt
    asciiNum >= 48 && asciiNum <= 57
  }

  def clear: Unit = {
    playersSequence.set(Vector.empty)
    playerIntervalSequence.set(Vector.empty)
    currentSequence.set(Vector.empty)
    currentIntervalSequence.set(Vector.empty)
  }

  def playerHasCorrectIntervals(playerIntervalVector: Vector[Int], intervals: Vector[Int]): Boolean = {
    //Note: The scala.math mod function gives wrong answers for negative numbers, so I'm using this Java function.
    //Of course the purpose is so that if a note, say 'E', is played in say C major, the player doesn't need to care what
    //octave he/she plays that tone in-- E will match E. (Really, the third degree of the major key will match the
    //third degree of the major key, no mather which octave the player chooses.) This makes it easier for player using a typing keyboard,
    //which is very limited in horizontal space, to give a response for an interval such as la to fa, where fa is higher than la.
    playerIntervalVector.map(x => java.lang.Math.floorMod(x, 12)).containsSlice(intervals.map(x => java.lang.Math.floorMod(x, 12)))

  }

  def createAndPlayNextTest: Unit = {
    Thread.sleep(timeToWaitAfterCorrestResponse)
    chooseAndSound(mostRecentOffsetForTestKeyboard, numTestIntervalsToPlay, timeToSoundTestNote)
  }

  val keyListener = new KeyListener {
    def keyTyped(e: KeyEvent): Unit = {
      //println(s"key typed is $e")
    }

    def keyPressed(e: KeyEvent): Unit = {

      val ch = e.getKeyChar

      if(isDigit(ch)) {
        //TODO Need to play the user's tone here, based on the interval

        val intervalGuess = ch.toInt - 48 match {
          case x if x < 0 => x * -1
          case x => x
        }
        println(s"player's latest interval is $intervalGuess")

        playerIntervalSequence.getAndUpdate { v =>
          v :+ intervalGuess
        }
        //I think with we don't seem to ever call playerHasCorrectIntervals from this postion, at least the way the game is played
        //in June 2025
        if(playerHasCorrectIntervals(playerIntervalSequence.get, currentIntervalSequence.get)) {
          println(s"player's interval(s) is/are correct")
          clear
          //Play last note of tester's previous sequence to set the player's starting note for the next set of intervals.
          //We might decide not to do this.
          //No longer sounding last note of player's sequence when it's correct.
          //Player.soundNotesForTime(Seq(comparisonTone + mostRecentOffsetForTestKeyboard), 300 )(testerChannel)

          createAndPlayNextTest
        } else if(playerIntervalSequence.get.size == currentIntervalSequence.get.size) {
          println(s"wrong guess. Actual sequence was ${currentIntervalSequence.get} and player's guess was  ${playerIntervalSequence.get}")
        }

      }

      else if (ch == 'A') { //A stands for "again", as in "play the sequence again"
        val prevOffset = previousOffsetForTestKeyboard
        val testNotes = currentSequence.get

        val intervals = testNotes.foldLeft( (Vector.empty[Int], prevOffset)   ){  (x, curr: Int) =>  x match {
          case (acc, prev) =>   (acc :+ (curr - prev),  curr)
        }  }._1

        println(s"test intervals are $intervals")
//No longer sounding previous note for comparison
        //Player.soundNotesForTime(Seq(comparisonTone + prevOffset), timeToSoundTestNote)(testerChannel)
        testNotes.foreach { case newOffset =>
          Player.soundNotesForTime(Seq(comparisonTone + newOffset), timeToSoundTestNote)(testerChannel)
        }
      }
      else {
        Keyboard.offset(ch).map { newOffsetForPlayer =>
          offsetForPlayerKeyboard = newOffsetForPlayer
          //Experiment: No longer play note that player tries
          //Player.turnOn(comparisonTone + offsetForPlayerKeyboard, playerChannel)
          playersSequence.getAndUpdate { v =>
            v :+ offsetForPlayerKeyboard
          }
          //TODO Watch Out. currentSequence and currentIntervalSequence give very different results.
          //Do we need currentSequence any more?
          if(playerHasCorrectIntervals(playersSequence.get, currentSequence.get)) {
            println(s"player is correct")
            clear
            createAndPlayNextTest
          } else if (playersSequence.get.size ==currentSequence.get.size ){
            println("Player is wrong")
          } else {
            //do nothing, player is still working on this sequence
          }

        }.getOrElse {
          println("not a valid note")

        }
      }

    }

    def keyReleased(e: KeyEvent): Unit = {
      println("key released")
      Player.turnOff(comparisonTone + offsetForPlayerKeyboard, playerChannel)
    }
  }

  val keyEventDemo = new KeyEventDemo(keyListener)
  keyEventDemo.startIt()

  //Just doing one interval at a time might not be that useful
  val start = 0
  //When the test chennal has a long sustain, for example, with instrument = 54, which sounds like a voice,
  //a short time between notes will caus you to get cues by harmonic warble.
  //This can be good or bad, depending on what you want to work on.
  //700 is a short enough time interval to generate a harmonic warble
  Player.soundNotesForTime(Seq(comparisonTone + start), timeToSoundTestNote)(testerChannel)
  chooseAndSound(start, numTestIntervalsToPlay, timeToSoundTestNote)

}

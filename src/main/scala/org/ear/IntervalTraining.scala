package org.ear

import java.util.concurrent.atomic.AtomicReference

import org.ear.NewMusicStuff.keyListener

object IntervalTraining extends App {

  //Setting 60 means that offsets are relative to middle C.
  //When comparisonTone is 60 and legalOffsets are offsets that describe
  //the diatonic tones in the major key, then tones will be generated in C Major.
  //If you change the comparision tone to 62 and legalOffsets stays the same,
  //then tones will be generated in D Major.
  val comparisonTone = 62 // 51
  //Settings
  //These legal offsets restrict the game tones to diatonic in the major key.
  val legalOffsets = Vector(-3,-1,0, 2, 4,5,7,9,11,12,14,16,17)

  import javax.sound.midi.MidiChannel
  import java.awt.event.KeyListener
  import java.awt.event.{KeyEvent, KeyListener}

  val synth = Player.createSynth
  //Settting controls the sound played by the player

  val playerChannel = Player.channelAndInstrument(synth, 1, 120) //120
  //Make this 0 if you only want to play the keyboard, rather than answer the tones that the test creates
  //Last CHANGED
  //Setting Controls the sound played by the tester
  //See https://en.wikipedia.org/wiki/General_MIDI Program Change Events for the sounds
  val testerChannel: MidiChannel = Player.channelAndInstrument(synth, 0, 105) //42, 17 and 28 are interesting too    // Player.makeChannels(0)

  //An offset is a difference from the 0 note
  //The intervals tell us the difference between each note and its preceding noe
  //The offsets tell us which actual note is being played (although we need to add some value to each offset to actually sound the a note)
  def chooseNextIntervalAndOffset(legalIntervals: Vector[Int], currentOffset: Int, lowestOffset: Int, highestOffset: Int): (Int, Int) = {
    val diff = Choosing.chooseRandomly(legalIntervals)
    val nextOffset = diff + currentOffset

    //TODO THIS legalOffsets.containst stuff S A quick shortcut to restrict the set of notes (not intervals, notes) that we choose from
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

  //val legalIntervals: Vector[Int]= (-5 to 5).toVector.filterNot(_ == 0)
  //Setting controls how far away the next note is allowed to be from the previous one
  val legalIntervals: Vector[Int]=  (-12 to 12).toVector.filterNot(_ == 0) //(-12 to 12).toVector.filterNot(_ == 0) //(-7 to 7).toVector.filterNot(_ == 0)
  //val legalIntervals: Vector[Int]= Vector(2, -2, 3,-3,4,-4)
  //val legalIntervals: Vector[Int]= Vector(1, -1, 2, -2, 5, -5, 6, -6, 7, -7)
  //val legalIntervals: Vector[Int]= (-9 to 9).toVector.filterNot(x => math.abs(x) < 5)
  //Interesting. Play John Coltrane's Favorite Things on You Tube, reduce the YouTube volume to 1/3 of normal,
  //and, for now, my error rates go from 98% or so to 25% or so at numTestIntersToPlay = 1
  //Last changed
  //Setting
  val numTestIntervalsToPlay = 4//3  //5
  //Important at speed quicker thann 800 I need to say the solfeg first before pressing the notes,
  //where at less challenging spaeds I often say the solfegg first, but I don't have to, because I
  //usually hear the solfeggio as the notes play. It's probably good to force that.
  //LAST CHANGED!
  //Setting
  val timeToSoundTestNote = 800//1000 //500//800 //400 //1000 //600 //1000 2000 //2000 4 at a time is good
  val timeToWaitAfterCorrestResponse = 300 //500

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
    if(playerIntervalVector.size < intervals.size)
      false
    else {
      playerIntervalVector.slice(playerIntervalVector.size - intervals.size, playerIntervalVector.size).zip(intervals).find{ case(guess, actual) =>
        guess != actual && guess != actual * (-1) //If interval is -3 and player said 3, we count that as correct
      }.isEmpty
    }

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
        if(playerHasCorrectIntervals(playerIntervalSequence.get, currentIntervalSequence.get)) {
          println(s"player's interval(s) is/are correct")
          clear
          //Play last note of tester's previous sequence to set the player's starting note for the next set of intervals.
          //We might decide not to do this.
          Player.soundNotesForTime(Seq(comparisonTone + mostRecentOffsetForTestKeyboard), timeToSoundTestNote )(testerChannel)

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

        Player.soundNotesForTime(Seq(comparisonTone + prevOffset), timeToSoundTestNote)(testerChannel)
        testNotes.foreach { case newOffset =>
          Player.soundNotesForTime(Seq(comparisonTone + newOffset), timeToSoundTestNote)(testerChannel)
        }
      }
      else {
        Keyboard.offset(ch).map { newOffsetForPlayer =>
          offsetForPlayerKeyboard = newOffsetForPlayer
          Player.turnOn(comparisonTone + offsetForPlayerKeyboard, playerChannel)
          playersSequence.getAndUpdate { v =>
            v :+ offsetForPlayerKeyboard
          }
          println(s"curentSequece is ${currentSequence.get} and playerSequence is ${playersSequence.get}")
          if (playersSequence.get.containsSlice(currentSequence.get)) {
            println(s"player is correct")

            clear
            createAndPlayNextTest
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
  Player.soundNotesForTime(Seq(comparisonTone + start), 600)(testerChannel)
  chooseAndSound(start, numTestIntervalsToPlay, timeToSoundTestNote)

}

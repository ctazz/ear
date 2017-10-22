package org.ear

import javax.sound.midi._


object SynthesizerPlayIt extends App {

  import Player._

  implicit val channel = makeChannels(0)


  turnOn(Seq(60, 64, 67))
  Thread.sleep(2000)
  turnOff(Seq(60, 64, 67))



}


object Player {

  def makeChannels: Array[MidiChannel] = {
    val midiSynth = MidiSystem.getSynthesizer
    midiSynth.open()

    //get and load default instrument and channel lists
    val instr = midiSynth.getDefaultSoundbank.getInstruments
    val channels = midiSynth.getChannels
    //Note: Shit still sounds the same no mater which channel I choose or which instrument I choose!

    midiSynth.loadInstrument(instr(120)) //load an instrument. For now they all sound the same

    channels

  }

  //For now we'll play all notes on the same channel
  def turnOn(notes: Seq[Int])(implicit channel: MidiChannel): Unit = {
    val theVelocity = 100
    notes.foreach(note => channel.noteOn(note, theVelocity)  )
  }

  def turnOff(notes: Seq[Int])(implicit channel: MidiChannel): Unit = {
    notes.foreach(channel.noteOff(_))
  }

  import java.util.{Timer,TimerTask}

  //Timer's are supposed to be shared across threads, apparently
  //But this isn't working when I do that!!!  I seem to need the function-local timer so far!!!
  //Maybe we need to be a trait or a class, not an Object
  //val timer = new Timer
  //TODO Probably need to have controlling code cancel this timer, because the timer holds a thread
/*  def cancelTimer: Unit = {
    timer.cancel
  }*/

  def soundNotesForTime(notes: Seq[Int], time: Long)(implicit channel: MidiChannel): Unit = {
    turnOn(notes)

    //PROBLEM:  When we do this, we don't block, and that means a subsequent call to soundNotesForTime plays at the same time as the first call!!!
/*    val timer = new Timer
    timer.schedule(
      new TimerTask {
        def run: Unit =
          turnOff(notes)
      }, time
    )*/

    Thread.sleep(time)
    turnOff(notes)

  }

  //java sucks
  import collection.JavaConverters._
  def makeChannelsAsJava: java.util.List[MidiChannel] = {
    makeChannels.toList.asJava
  }

  def turnOn(note: Int, channel: MidiChannel): Unit = {
    turnOn(Seq(note))(channel)
  }

  def turnOff(note: Int, channel: MidiChannel): Unit = {
    turnOff(Seq(note))(channel)
  }


}
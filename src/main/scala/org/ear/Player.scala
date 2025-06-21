package org.ear

import javax.sound.midi._


object SynthesizerPlayIt extends App {

  import Player._

  implicit val channel = makeChannels(0)


  turnOn(Seq(60, 64, 67))
  Thread.sleep(2000)
  turnOff(Seq(60, 64, 67))



}

//See https://stackoverflow.com/questions/4881541/java-midi-synthesizer-cant-change-instruments
object Player {

  //More stuff happens in here now than can be represented by our interface.
  //Need to pass in the channel Int you want and the instrument Int, and we return that channel
  //with that instrument loaded on it
  //channelAndInstrument is the better function now
  def makeChannels: Array[MidiChannel] = {
    val synth = MidiSystem.getSynthesizer
    synth.open()

    //get and load default instrument and channel lists
    val instr = synth.getDefaultSoundbank.getInstruments
    val channels = synth.getChannels

    //I wonder if this line is even needed!
    //synth.loadInstrument(instr(99)) //load an instrument.

    //mc[4].programChange(instr[120].getPatch().getProgram());
    //120 has a very short sustain, which is good for test chords meshing with the user's answer
    //Maybe I should give user and player different instruments
    channels(0).programChange(instr(0).getPatch.getProgram)

    channels

  }

  def channelAndInstrument(synth: Synthesizer, channelNum: Int, instrumentNum: Int): MidiChannel = {
    //get and load default instrument and channel lists
    val instruments = synth.getDefaultSoundbank.getInstruments
    val channels = synth.getChannels

    //I wonder if this line is even needed!
    synth.loadInstrument(instruments(instrumentNum)) //load an instrument.

    //mc[4].programChange(instr[120].getPatch().getProgram());
    //120 has a very short sustain, which is good for test chords meshing with the user's answer
    //Maybe I should give user and player different instruments
    channels(channelNum).programChange(instruments(instrumentNum).getPatch.getProgram)

    channels(channelNum)
  }

  def createSynth: Synthesizer = {
    val synth = MidiSystem.getSynthesizer
    synth.open()
    synth
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

  def soundNotesForTimeAsync(notes: Seq[Int], time: Long)(implicit channel: MidiChannel): Unit = {
    turnOn(notes)
    val timer = new Timer
    timer.schedule(
      new TimerTask {
        def run: Unit = {
          println("Turning off")
          turnOff(notes)
        }
      }, time
    )
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
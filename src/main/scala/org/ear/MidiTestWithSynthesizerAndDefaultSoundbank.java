package org.ear;

import javax.sound.midi.*;

//Guy in stack overflow recommends these midi tutorials.
//http://www.ibm.com/developerworks/library/it/it-0801art38/
//http://patater.com/gbaguy/javamidi.htm   Says this one, by a 15 year old with non-working code, is the most useful

public class MidiTestWithSynthesizerAndDefaultSoundbank {

    public static void main(String[] args) {
        try{
        /* Create a new Sythesizer and open it. Most of
         * the methods you will want to use to expand on this
         * example can be found in the Java documentation here:
         * https://docs.oracle.com/javase/7/docs/api/javax/sound/midi/Synthesizer.html
         */
            Synthesizer midiSynth = MidiSystem.getSynthesizer();
            midiSynth.open();

            //get and load default instrument and channel lists
            Instrument[] instr = midiSynth.getDefaultSoundbank().getInstruments();
            MidiChannel[] mChannels = midiSynth.getChannels();
            //Note: Shit still sounds the same no mater which channel I choose or which instrument I choose!

            midiSynth.loadInstrument(instr[120]);//load an instrument


            mChannels[0].noteOn(60, 100);//On channel 0, play note number 60 with velocity 100
            mChannels[0].noteOn(64, 100);
            try { Thread.sleep(100); // wait time in milliseconds to control duration
            } catch( InterruptedException e ) { }
            mChannels[0].noteOff(60);//turn of the note
            mChannels[0].noteOff(64);//turn of the note


        } catch (MidiUnavailableException e) {}
    }

}
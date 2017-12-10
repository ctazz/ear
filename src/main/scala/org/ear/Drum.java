package org.ear;

import javax.sound.midi.*;
//import javax.sound.*;
//This is some example I got somewhere
public class Drum {
    static int instrument = 45;
    static int note = 100;
    static int timbre = 0;
    static int force = 100;

    public static void main(String[] args) {
        Synthesizer synth = null;
        try {
            synth = MidiSystem.getSynthesizer();
            synth.open();
        } catch (Exception e) {
            System.out.println(e);
        }
        Soundbank soundbank = synth.getDefaultSoundbank();
        Instrument[] instr = soundbank.getInstruments();
        synth.loadInstrument(instr[instrument]);    //Changing this int (instrument) does nothing
        MidiChannel[] mc = synth.getChannels();
        //mc[4].programChange(instr.getPatch().getProgram());
        mc[4].programChange(instr[120].getPatch().getProgram());
        mc[4].noteOn(note, force);
        try {
            Thread.sleep(4000);
        } catch (InterruptedException e) {
        }
        System.out.println(instr[instrument].getName());

        synth.close();

    }
}
#include <AudioUnit/AudioUnit.h>
#include <AudioToolbox/AudioToolbox.h> //for AUGraph

// This file is a C version of The C++ PlaySoftMidi.cpp

// some MIDI constants:


char *instruments[] =
    {"Acoustic Grand Piano", "Bright Acoustic Piano",
     "Electric Grand Piano", "Honky-tonk Piano",
     "Electric Piano 1", "Electric Piano 2", "Harpsichord",
     "Clavi", "Celesta", "Glockenspiel", "Music Box",
     "Vibraphone", "Marimba", "Xylophone", "Tubular Bells",
     "Dulcimer", "Drawbar Organ", "Percussive Organ",
     "Rock Organ", "Church Organ", "Reed Organ",
     "Accordion", "Harmonica", "Tango Accordion",
     "Acoustic Guitar (nylon)", "Acoustic Guitar (steel)",
     "Electric Guitar (jazz)", "Electric Guitar (clean)",
     "Electric Guitar (muted)", "Overdriven Guitar",
     "Distortion Guitar", "Guitar harmonics",
     "Acoustic Bass", "Electric Bass (finger)",
     "Electric Bass (pick)", "Fretless Bass",
     "Slap Bass 1", "Slap Bass 2", "Synth Bass 1",
     "Synth Bass 2", "Violin", "Viola", "Cello",
     "Contrabass", "Tremolo Strings", "Pizzicato Strings",
     "Orchestral Harp", "Timpani", "String Ensemble 1",
     "String Ensemble 2", "SynthStrings 1", "SynthStrings 2",
     "Choir Aahs", "Voice Oohs", "Synth Voice",
     "Orchestra Hit", "Trumpet", "Trombone", "Tuba",
     "Muted Trumpet", "French Horn", "Brass Section",
     "SynthBrass 1", "SynthBrass 2", "Soprano Sax",
     "Alto Sax", "Tenor Sax", "Baritone Sax", "Oboe",
     "English Horn", "Bassoon", "Clarinet", "Piccolo",
     "Flute", "Recorder", "Pan Flute", "Blown Bottle",
     "Shakuhachi", "Whistle", "Ocarina", "Lead 1 (square)",
     "Lead 2 (sawtooth)", "Lead 3 (calliope)", "Lead 4 (chiff)",
     "Lead 5 (charang)", "Lead 6 (voice)", "Lead 7 (fifths)",
     "Lead 8 (bass + lead)", "Pad 1 (new age)", "Pad 2 (warm)",
     "Pad 3 (polysynth)", "Pad 4 (choir)", "Pad 5 (bowed)",
     "Pad 6 (metallic)", "Pad 7 (halo)", "Pad 8 (sweep)",
     "FX 1 (rain)", "FX 2 (soundtrack)", "FX 3 (crystal)",
     "FX 4 (atmosphere)", "FX 5 (brightness)", "FX 6 (goblins)",
     "FX 7 (echoes)", "FX 8 (sci-fi)", "Sitar", "Banjo",
     "Shamisen", "Koto", "Kalimba", "Bag pipe", "Fiddle",
     "Shanai", "Tinkle Bell", "Agogo", "Steel Drums",
     "Woodblock", "Taiko Drum", "Melodic Tom", "Synth Drum",
     "Reverse Cymbal", "Guitar Fret Noise", "Breath Noise",
     "Seashore", "Bird Tweet", "Telephone Ring",
     "Helicopter", "Applause", "Gunshot"};

enum {
  kMidiMessage_ControlChange 		= 0xB,
  kMidiMessage_ProgramChange 		= 0xC,
  kMidiMessage_BankMSBControl 	= 0,
  kMidiMessage_BankLSBControl		= 32,
  kMidiMessage_NoteOn 			= 0x9,
  kMidiMessageProgramChange = 0xC0
  


};


int main (int argc, const char * argv[]) {
  AUGraph outGraph;
  AUNode synthNode, limiterNode, outNode;
  AudioUnit outSynth;
  int i;
  OSStatus result;
  AudioComponentDescription cd;
  UInt8 midiChannelInUse = 0; //we're using midi channel 1...

  result = NewAUGraph(&outGraph);
  printf("result1 = %i\n", result);
  
  cd.componentManufacturer = kAudioUnitManufacturer_Apple;
  cd.componentFlags = 0;
  cd.componentFlagsMask = 0;
	
  cd.componentType = kAudioUnitType_MusicDevice;
  cd.componentSubType = kAudioUnitSubType_DLSSynth;
  result = AUGraphAddNode (outGraph, &cd, &synthNode);
  printf("result2 = %i\n", result);


  cd.componentType = kAudioUnitType_Effect;
  cd.componentSubType = kAudioUnitSubType_PeakLimiter;  

  result = AUGraphAddNode(outGraph, &cd, &limiterNode);
  printf("result3 = %i\n", result);

  cd.componentType = kAudioUnitType_Output;
  cd.componentSubType = kAudioUnitSubType_DefaultOutput;  
  result = AUGraphAddNode (outGraph, &cd, &outNode);
  printf("result4 = %i\n", result);

  result = AUGraphOpen (outGraph);
  printf("result5 = %i\n", result);
	
  result = AUGraphConnectNodeInput (outGraph, synthNode, 0, limiterNode, 0);
  printf("result6 = %i\n", result);
  result = AUGraphConnectNodeInput (outGraph, limiterNode, 0, outNode, 0);
  printf("result7 = %i\n", result);
	
  // ok we're good to go - get the Synth Unit...
  result = AUGraphNodeInfo(outGraph, synthNode, 0, &outSynth);
  printf("result8 = %i\n", result);
  
  result = AUGraphInitialize (outGraph);
  printf("result9 = %i\n", result);
  
  result = MusicDeviceMIDIEvent(outSynth, 
				kMidiMessage_ControlChange << 4 | midiChannelInUse, 
				kMidiMessage_BankMSBControl, 0,
				0/*sample offset*/);
  printf("result10 = %i\n", result);

  result = MusicDeviceMIDIEvent(outSynth, 
				kMidiMessage_ProgramChange << 4 | midiChannelInUse, 
				0/*prog change num*/, 0,
				0/*sample offset*/);
  printf("result11 = %i\n", result);
	
  CAShow (outGraph); // prints out the graph so we can see what it looks like...
	
  result = AUGraphStart (outGraph);
  printf("result12 = %i\n", result);

  int instrument = 2; //paino
  instrument = 24;
  printf("instrument %s\n", instruments[instrument]);
  MusicDeviceMIDIEvent(outSynth, kMidiMessageProgramChange,
		       instrument, 0, 0);

  // we're going to play an octave of MIDI notes: one a second
  for (i = 0; i < 127; i++) {
    UInt32 noteNum = i;
    UInt32 onVelocity = 127;
    UInt32 noteOnCommand = 	kMidiMessage_NoteOn << 4 | midiChannelInUse;
    
    printf ("Playing Note: Status: %i, Note: %i, Vel: %i\n", 
	    noteOnCommand, noteNum, onVelocity);
	  
    result = MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, onVelocity, 0);
    // sleep for a 1/20 second
    usleep (1 * 1000 * 50);

    result = MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, 0, 0);
  };

  for(instrument = 1; instrument < 127; instrument++){
    printf("instrument %s\n", instruments[instrument]);
    MusicDeviceMIDIEvent(outSynth, kMidiMessageProgramChange,
			 instrument, 0, 0);
  
    for (i = 0; i < 10 ; i++) {
      UInt32 noteNum = i+60;
      UInt32 onVelocity = 127;
      UInt32 noteOnCommand = 	kMidiMessage_NoteOn << 4 | midiChannelInUse;
      MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, onVelocity, 0);
      // sleep for a 1/20 second
      usleep (1 * 1000 * 250);
      
      result = MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, 0, 0);
    };
  };
  

  return 0;

}

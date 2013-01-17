-module(au_nifs).
-compile(export_all).
-on_load(load_lib/0).
-import(lists, [reverse/1]).

-define(kMidiMessage_ControlChange, 16#B).
-define(kMidiMessage_ProgramChange, 16#C).
-define(kMidiMessage_BankMSBControl, 0).
-define(kMidiMessage_BankLSBControl, 32).
-define(kMidiMessage_NoteOn,16#9).
-define(kMidiMessageProgramChange,16#C0).

test() ->
    L = all_aus(),
    %% elib1_misc:dump("au.tmp",L),
    io:format("L=~p~n#devices = ~p~n",[L,length(L)]).

test1() ->
    Synth = do_setup(),
    %% io:format("Synth=~p~n",[Synth]),
    play(Synth).

do_setup() ->
    %% io:format("make_au_graph~n"),
    OutGraph = make_au_graph(),
    %% io:format("OutGraph=~p~n",[OutGraph]),
    SynthNode = my_graph_add_node(OutGraph, {<<"aumu">>,<<"dls ">>,<<"appl">>}),
    %% io:format("SynthNode=~p~n",[SynthNode]),
    Limiter = my_graph_add_node(OutGraph, {<<"aufx">>,<<"lmtr">>,<<"appl">>}),
    %% io:format("Limiter=~p~n",[Limiter]),
    Out = my_graph_add_node(OutGraph, {<<"auou">>,<<"def ">>,<<"appl">>}),
    %% io:format("Out=~p~n",[Out]),
    Result = au_graph_open(OutGraph),
    %% io:format("here 12 Result=~p~n",[Result]),
    au_graph_connect_node_input(OutGraph, SynthNode, 0, Limiter, 0),
    au_graph_connect_node_input(OutGraph, Limiter, 0, Out, 0),
    OutSynth = au_graph_node_info(OutGraph, SynthNode, 0),
    au_graph_initialize(OutGraph),
    %% io:format("so far~n"),
    %% music_device_midi_event(OutSynth, 176, 0, 0,0),
    %% music_device_midi_event(OutSynth, 192, 0, 0,0),
    au_graph_start(OutGraph),
    OutSynth.

play(OutSynth) ->
    ProgChange=12,
    Instrument=1,
    Channel = 0, 
    %% music_device_midi_event(OutSynth, ProgChange, Instrument, 0,0),
    %% music_device_midi_event(OutSynth,
    %% 			    (?kMidiMessage_ControlChange bsl 4) bor Channel, 
    %% 			    ?kMidiMessage_BankMSBControl, 
    %% 			    0, 0),
    %% music_device_midi_event(OutSynth,
    %% 			    (?kMidiMessage_ProgramChange bsl 4) bor Channel, 
    %% 			    0,0,0),
    io:format("Instrument = Piano~n"),
    set_instrument(OutSynth, Instrument),
    NoteOnCommand = (?kMidiMessage_NoteOn bsl 4) bor Channel,
    %% music_device_midi_event(OutSynth, NoteOnCommand, 20, 127, 0),
    io:format("Fast scales~n"),
    for(fun(I) ->
		note_on(OutSynth, Channel, I, 127),
		sleep(50),
		note_off(OutSynth, Channel, I)
	end, 20, 80),
    io:format("All the instruments~n"),
    for(fun(I) ->
		set_instrument(OutSynth, I),
		note_on(OutSynth, Channel, 50, 127),
		sleep(50),
		note_off(OutSynth, Channel,50)
	end, 1, 120),
    true.

note_off(Synt, Channel, Pitch) ->
    note_on(Synt, Channel, Pitch, 0).

note_on(Synt, Channel, Pitch, Vol) ->
    NoteOnCommand = (?kMidiMessage_NoteOn bsl 4) bor Channel,
    music_device_midi_event(Synt, NoteOnCommand, Pitch, Vol, 0).

set_instrument(Synth, Instrument) ->
    music_device_midi_event(Synth, ?kMidiMessageProgramChange,
			    Instrument, 0, 0).

for(F, I, I) -> F(I);
for(F, I, J) -> F(I),for(F,I+1,J).

sleep(T) ->
    receive
    after T ->
	    true
    end.

au_graph_start(_) ->
    void.

music_device_midi_event(_A,_B,_C,_D,_E) ->
    void.

au_graph_initialize(_) ->
    void.

au_graph_node_info(_,_,_) ->
    void.

    %%.AUGraphConnectNodeInput (

au_graph_connect_node_input(_,_,_,_,_) ->
    void.

au_init() ->
    void.

my_graph_add_node(G, {<<Type:32>>,<<SubType:32>>,<<Manu:32>>}) ->
    %% io:format("au_graph_add_node: Type:~p Sub:~p Man:~p~n",[Type,SubType,Manu]),
    Val = au_graph_add_node(G, {Type, SubType, Manu}),
    %% io:format("here mygarph add node=~p~n",[Val]),
    Val.

au_graph_open(_) ->
    true.

au_graph_add_node(_, _) ->
    dummy.

make_au_graph() ->
    dummy.

load_lib() ->
    %% io:format("load_lib~n"),
    Z = erlang:load_nif("./au_nifs", 0),
    %% erlang:display({z,Z}),
    %% io:format("load_lib done:~p~n",[Z]).
    ok.
    
all_aus() ->
    lists:flatten([list_aus(I) || I <- au_types()]).

au_types() ->
    [<<"aufx">>,<<"aumf">>,<<"auol">>,<<"aumu">>,<<"augn">>,<<"aufc">>,<<"aumx">>,
     <<"aupn">>,<<"auou">>].

list_aus(<<Int:32>>) ->
    L = nif_list_type(Int),
    [{<<A:32>>,<<B:32>>,<<C:32>>,printable(D)} || {A,B,C,D} <- L].

printable(L) -> lists:filter(fun is_printable/1, L).

is_printable(H) when $a =< H, H =< $z -> true;
is_printable(H) when $A =< H, H =< $Z -> true;
is_printable(H) when $0 =< H, H =< $9 -> true;
is_printable(X) -> lists:member(X, " :-.()[]{}<>;").

nif_list_type(_) ->
    fooy.

%% The Audio Unit Specification defines the plug-in API for the
%% following audio unit types:

%% Effect units('aufx'),such as volume controls, equalizers, and
%% reverbs, which modify an audio data stream

%% Music effect units ('aumf'), such as loopers, which combine features
%% of instrument units (such as starting

%% and stopping a sample) with features of effect units

%% Offline effect units ('auol'), which let you do things with audio
%% that aren't practical in real time, such as time reversal or
%% look-ahead level normalization

%% Instrument units ('aumu'), which take MIDI and soundbank data as
%% input and provide audio data as outputl etting a user play a virtual
%% instrument

%% Generator units ('augn'), which programmatically generate an audio
%% data stream or play audio from a file

%% Data format converter units ('aufc'), which change characteristics
%% of an audio data stream such as bit depth, sample rate, or playback
%% speed

%% Mixer units ('aumx'), which combine audio data streams

%% Panner units ('aupn'), which distribute a set of input channels,
%% using a spatialization algorithm, to a set of output channels

%% output form auval -a

%% aufx bpas appl  -  Apple: AUBandpass
%% aufx dcmp appl  -  Apple: AUDynamicsProcessor
%% aufx dely appl  -  Apple: AUDelay
%% aufx dist appl  -  Apple: AUDistortion
%% aufx filt appl  -  Apple: AUFilter
%% aufx greq appl  -  Apple: AUGraphicEQ
%% aufx hpas appl  -  Apple: AUHipass
%% aufx hshf appl  -  Apple: AUHighShelfFilter
%% aufx lmtr appl  -  Apple: AUPeakLimiter
%% aufx lpas appl  -  Apple: AULowpass
%% aufx lshf appl  -  Apple: AULowShelfFilter
%% aufx mcmp appl  -  Apple: AUMultibandCompressor
%% aufx mrev appl  -  Apple: AUMatrixReverb
%% aufx nsnd appl  -  Apple: AUNetSend
%% aufx pmeq appl  -  Apple: AUParametricEQ
%% aufx rogr appl  -  Apple: AURogerBeep
%% aufx sdly appl  -  Apple: AUSampleDelay
%% aufx tmpt appl  -  Apple: AUPitch
%% aufx JASb ElCa  -  jackosx.com: JACK-insert
%% aufx PpPv PSPa  -  PSPaudioware: PSP PianoVerb
%% aupn ambi appl  -  Apple: AUSoundFieldPanner
%% aupn hrtf appl  -  Apple: HRTFPanner
%% aupn sphr appl  -  Apple: AUSphericalHeadPanner
%% aupn vbas appl  -  Apple: AUVectorPanner
%% aumu dls  appl  -  Apple: DLSMusicDevice
%% aumu samp appl  -  Apple: AUSampler
%% aumu GARA GaRR  -  Garritan: ARA Player
%% aumu Pt3q Mdrt  -  Modartt: Pianoteq Trial 3
%% aumx 3dmx appl  -  Apple: AUMixer3D
%% aumx mcmx appl  -  Apple: AUMultiChannelMixer
%% aumx mxmx appl  -  Apple: AUMatrixMixer
%% aumx smxr appl  -  Apple: AUMixer
%% aufc conv appl  -  Apple: AUConverter
%% aufc defr appl  -  Apple: AUDeferredRenderer
%% aufc merg appl  -  Apple: AUMerger
%% aufc nutp appl  -  Apple: AUNewTimePitch
%% aufc splt appl  -  Apple: AUSplitter
%% aufc tmpt appl  -  Apple: AUTimePitch
%% aufc vari appl  -  Apple: AUVarispeed
%% aufc raac appl  -  Apple: AURoundTripAAC
%% auou ahal appl  -  Apple: AudioDeviceOutput
%% auou def  appl  -  Apple: DefaultOutputUnit
%% auou genr appl  -  Apple: GenericOutput
%% auou sys  appl  -  Apple: SystemOutputUnit
%% auou vpio appl  -  Apple: AUVoiceProcessor
%% augn afpl appl  -  Apple: AUAudioFilePlayer
%% augn nrcv appl  -  Apple: AUNetReceive
%% augn sspl appl  -  Apple: AUScheduledSoundPlayer
%% augn ttsp appl  -  Apple: AUSpeechSynthesis
%% auol tmpt appl  -  Apple: AUTimePitch
%% auol vari appl  -  Apple: AUVarispeed

%% 75-125 = 51

%% Macintosh:au joe$ 

%% {<<"aufx">>,<<"dcmp">>,<<"appl">>,
%%     [65,112,112,108,101,58,32,65,85,68,121,110,97,109,105,99,115,80,114,111,
%%      99,101,115,115,111,114,1,27]}, $$ 1,27 at end
%%     [65,112,112,108,101,58,32,65,85,68,121,110,97,109,105,99, 115,80,114,111,
%%      99,101,115,115,111,114,
%% "Apple: AUDynamicsProcessor"++[a].
%% aufx dcmp appl  -  Apple: AUDynamicsProcessor


%% https://developer.apple.com/library/mac/#documentation/AudioUnit/Reference/
%%       AUComponentServicesReference/Reference/reference.html

%% Audio Unit Types
%% The defined types of audio processing plug-ins known as audio units.

%% enum {
%%    kAudioUnitType_Output            = 'auou',
%%    kAudioUnitType_MusicDevice       = 'aumu',
%%    kAudioUnitType_MusicEffect       = 'aumf',
%%    kAudioUnitType_FormatConverter   = 'aufc',
%%    kAudioUnitType_Effect            = 'aufx',
%%    kAudioUnitType_Mixer             = 'aumx',
%%    kAudioUnitType_Panner            = 'aupn',
%%    kAudioUnitType_OfflineEffect     = 'auol',
%%    kAudioUnitType_Generator         = 'augn',
%% };

%% Constants
%% Page 29 of AUComponentServicesReference.pdf
%%
%% 'auou' kAudioUnitType_Output - An output unit provides input, output,
%% or both input and output simultaneously. It can be used as the head of
%% an audio unit processing graph. Available in OS X v10.2 and later.
%% Declared in AUComponent.h.

%% 'aumu' kAudioUnitType_MusicDevice - An instrument unit can be used as a
%% software musical instrument, such as a sampler or synthesizer. It
%% responds to MIDI (Musical Instrument Digital Interface) control
%% signals and can create notes.  Available in OS X v10.2 and later.
%% Declared in AUComponent.h.  

%% 'aumf' kAudioUnitType_MusicEffect An effect unit that can respond to MIDI
%% control messages, typically through a mapping of MIDI messages to
%% parameters of the audio unit's DSP algorithm.  Available in OS X v10.2
%% and later.  Declared in AUComponent.h.  

%% 'aufc' kAudioUnitType_FormatConverter
%% A format converter unit can transform audio formats, such as
%% performing sample rate conversion. A format converter is also
%% appropriate for deferred rendering and for effects such as
%% varispeed. A format converter unit can ask for as much or as little
%% audio input as it needs to produce a given output, while still
%% completing its rendering within the time represented by the output
%% buffer. For effect-like format converters, such as pitch shifters, it
%% is common to provide both a realtime and an offline version. OS X, for
%% example, includes Time-Pitch and Varispeed audio units in both
%% realtime and offline versions.  Available in OS X v10.2 and later.
%% Declared in AUComponent.h.  

%% 'aufx' kAudioUnitType_Effect An effect unit repeatedly processes a number of
%% audio input samples to produce the same number of audio output
%% samples. Most commonly, an effect unit has a single input and a single
%% output. Some effects take side-chain inputs as well. Effect units can
%% be run offline, such as to process a file without playing it, but are
%% expected to run in realtime.  Available in OS X v10.2 and later.
%% Declared in AUComponent.h.  

%% 'aumx' kAudioUnitType_Mixer A mixer unit takes a
%% number of input channels and mixes them to provide one or more output
%% channels. For example, the kAudioUnitSubType_StereoMixer audio unit in
%% OS X takes multiple mono or stereo inputs and produce a single stereo
%% output.  Available in OS X v10.2 and later.  Declared in
%% AUComponent.h.  

%% 'aupn' kAudioUnitType_Panner A panner unit is a specialized
%% effect unit that distributes one or more channels in a single input to
%% one or more channels in a single output. Panner units must support a
%% set of standard audio unit parameters that specify panning
%% coordinates.  Available in OS X v10.3 and later.  Declared in
%% AUComponent.h.  

%% 'kuol' kAudioUnitType_OfflineEffect An offline effect unit
%% provides digital signal processing of a sort that cannot proceed in
%% realtime. For example, level normalization requires examination of an
%% entire sound, beginning to end, before the normalization factor can be
%% calculated. As such, offline effect units also have a notion of a
%% priming stage that can be performed before the actual
%% rendering/processing phase is executed.  Available in OS X v10.3 and
%% later.  Declared in AUComponent.h.  

%% 'augn' kAudioUnitType_Generator A
%% generator unit provides audio output but has no audio input. This
%% audio unit type is appropriate for a tone generator. Unlike an
%% instrument unit, a generator unit does not have a control input.
%% Available in OS X v10.3 and later.  Declared in AUComponent.h.

instruments() ->
    ["Acoustic Grand Piano", "Bright Acoustic Piano",
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
     "Helicopter", "Applause", "Gunshot"].

  

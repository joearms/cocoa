-module(midi_nifs).
-compile(export_all).
-on_load(load_lib/0).
-import(lists, [reverse/1]).

list_devices() ->
    {Sources,Dests,Devices} = list_midi_devices(),
    %% these are in the reverse order
    {reverse(Sources), reverse(Dests), reverse(Devices)}.

load_lib() ->
    Z = erlang:load_nif("./midi_nifs", 0),
    erlang:display({z,Z}),
    Z.

test() ->
    list_midi_devices().

alloc(X) ->  
    a.

set(_,_,_) ->
    set.

get(_,_) ->
    get.

list_midi_devices() ->
    "list_deviced not loaded".

list_audio_components() ->
    foo.

connect_to_midi_source(I, Pid) ->
    bar.



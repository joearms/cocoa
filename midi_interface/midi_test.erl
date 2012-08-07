-module(midi_test).
-compile(export_all).

start() ->
    io:format("Starting midi tests ...~n"),
    io:format("Finding devices ...~n"),
    D = midi_nifs:list_devices(),
    io:format("Devices=~p~n",[D]),
    io:format("Checking for VMPK Output~n"),
    C = midi_nifs:list_audio_components(),
    io:format("# Audio components=~p~n",[C]),
    N = find_vmpk(D),
    io:format("VPMK on source:~p~n",[N]),
    Pid = spawn(fun() -> handle() end),
    E = midi_nifs:connect_to_midi_source(N, Pid),
    %% io:format("E=~p~n",[E]),
    receive
	after 100000 ->
		init:stop()
	end.

find_vmpk({Sources,_,_}) ->
    find_vmpk("VMPK Output", Sources, 0).

find_vmpk(H, [H|T], N) -> N;
find_vmpk(H, [_|T], N) -> find_vmpk(H, T, N+1);
find_vmpk(_, [], _)    -> exit(noVPMK).

handle() ->
    process_flag(trap_exit, true),
    loop().

loop() ->
    receive
	L ->
	    L1 = [lists:reverse(I) || I <- L],
	    io:format("Yes=~p~n",[L1]),
	    loop()
    end.

    

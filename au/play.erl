-module(play).
-compile(export_all).

start() ->
    init(),
    play_chords().

init() ->
    S = au_nifs:do_setup(),
    Instrument=1,
    Channel = 0, 
    au_nifs:set_instrument(S, Instrument),
    put(s, S).

play_chords() ->
    play_chords(60, chords:chords()).

play_chords(Note, [H|T]) ->
    io:format("Play:~p~n",[H]),
    L = chords:notes(H),
    L1 = [I + Note - 1 || I <- L],
    start(L1),
    sleep(400),
    stop(L1),
    sleep(200),
    play_chords(Note, T);
play_chords(_, []) ->
    true.

start(L) ->
    S = get(s),
    [au_nifs:note_on(S,0,I,127) || I<- L].

stop(L) ->
    S = get(s),
    [au_nifs:note_off(S,0,I) || I<- L].

sleep(T) ->
    receive
    after T ->
	    true
    end.





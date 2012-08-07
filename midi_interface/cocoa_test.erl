-module(cocoa_test).
-compile(export_all).

start() ->
    io:format("Starting cocoa tests ...~n"),
    Pid = spawn(fun() -> handle() end),
    E = cocoa_nifs:connect_to_cocoa(Pid),
    io:format("E=~p~n",[E]),
    receive
	after 100000 ->
		init:stop()
	end.

handle() ->
    process_flag(trap_exit, true),
    loop().

loop() ->
    receive
	L ->
	    io:format("Yes=~p~n",[L]),
	    loop()
    end.

    

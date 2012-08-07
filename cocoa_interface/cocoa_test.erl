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
    loop(0).

loop(N) ->
    receive
	{register, Name, Id} ->
	    io:format("register ~p=>~p~n",[Name,Id]),
	    put({register,Name}, Id),
	    loop(N);
	{click, _, Id} ->
	    io:format("clicked:~p~n",[Id]),
	    Id1 = get({register,text}),
	    cocoa_nifs:send_cocoa(Id1, "setStringValue:", 
				  "hello world:" ++ integer_to_list(N)),
	    loop(N+1);
	L ->
	    io:format("Yes=~p~n",[L]),
	    loop(N)
    end.

    

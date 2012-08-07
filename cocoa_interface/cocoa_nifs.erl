-module(cocoa_nifs).
-compile(export_all).
-on_load(load_lib/0).

load_lib() ->
    Z = erlang:load_nif("./cocoa_nifs", 0),
    erlang:display({z,Z}),
    Z.

connect_to_cocoa(_) ->
    connect_to_cocoa.

send_cocoa(_,_,_) ->
    send.


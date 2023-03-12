-module(a).

-export([
    start/1
]).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).


loop(Tag) ->
    sleep(),
    L = b:hello(),
    io:format("~s~n", [L]),
    loop(Tag).

sleep() ->
    receive
        after 3000 -> true
    end.

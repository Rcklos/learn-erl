-module(b).

-export([
    hello/0
]).

hello() ->
    <<"hello new b"/utf8>>.

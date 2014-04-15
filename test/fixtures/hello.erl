-module(hello).

-compile({parse_transform, import_as}).

-import_as([{io, cool}, {lists, l}]).

-export([main/0]).

main() ->
    cool:format("Hello PT!~n"),
    A = 3,
    if 
        A =:= 3 -> cool:format("A == 3~n");
        true -> cool:format("A != 3~n")
    end,
    cool:format("Print lists: ~p~n", [l:reverse([1,2,3,4,5])]).

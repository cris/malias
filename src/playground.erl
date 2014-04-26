-module(playground).

-compile(export_all).

test_iter() ->
    Matcher = fun({_,A,B}) ->
            fun({_,X,Y}) -> X =/= A orelse Y =/= B end
    end,
    List = [{2,a,b},{2,c,d},{2,a,b},{2,e,f},{2,e,f}],
    Elements = iterate_with_tail(List, Matcher, []),
    io:format("~p~n", [Elements]).

iterate_with_tail([], _Matcher, Acc) ->
    Acc;
iterate_with_tail([H|T], Matcher, Acc) ->
    Elements = match_all(H, T, Matcher),
    iterate_with_tail(T, Matcher, Elements ++ Acc).

match_all(H, T, Matcher) ->
    io:format("match_all: ~p~n", [H]),
    case lists:dropwhile(Matcher(H), T) of
        [] -> [];
        _ -> [H]
    end.

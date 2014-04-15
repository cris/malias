-module(import_as_tests).
-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, import_as}).
-import_as([{lists, l}, {string, s}]).
-import_as({proplists, pl}).

lists_import_test() ->
    Seq = l:seq(1, 5),
    ?assertEqual([1,2,3,4,5], Seq).

string_import_test() ->
    Len = s:len("12345"),
    ?assertEqual(5, Len).

one_tuple_import_test() ->
    Tuple = pl:lookup(b, [{a,1}, {b,2}, {c,3}]),
    ?assertEqual({b,2}, Tuple).

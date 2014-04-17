-module(malias_tests).
-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, malias}).
-malias([{lists, l}, {string, s}]).
-malias({proplists, pl}).

lists_import_test() ->
    Seq = l:seq(1, 5),
    ?assertEqual([1,2,3,4,5], Seq).

string_import_test() ->
    Len = s:len("12345"),
    ?assertEqual(5, Len).

one_tuple_import_test() ->
    Tuple = pl:lookup(b, [{a,1}, {b,2}, {c,3}]),
    ?assertEqual({b,2}, Tuple).

% error cases
incorrect_term_test() ->
    Forms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,1234}
    ],
    Description = io_lib:format("Incorrect parameter for malias: ~p~n", [1234]),
    ExpectedEForms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {error,{2,malias,Description}}
    ],
    EForms = malias:parse_transform(Forms, []),
    ?assertMatch(ExpectedEForms, EForms).

% error cases
incorrect_list_test() ->
    Param = [{1,2}],
    Forms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param}
    ],
    Description = io_lib:format("Incorrect parameter for malias: ~p~n", [Param]),
    ExpectedEForms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {error,{2,malias,Description}}
    ],
    EForms = malias:parse_transform(Forms, []),
    ?assertMatch(ExpectedEForms, EForms).

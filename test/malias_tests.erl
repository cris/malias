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

duplicated_items_same_list_test() ->
    Param = [{a,b}, {c,d}, {a,b}, {e,f}, {e,f}],
    Forms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param}
    ],
    Description = io_lib:format("Duplicates in malias: ~p, ~p", [{a,b}, {e,f}]),
    ExpectedEForms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param},
        {warning,{2,malias,Description}}
    ],
    EForms = malias:parse_transform(Forms, []),
    ?assertMatch(ExpectedEForms, EForms).

cross_duplicated_items_test() ->
    Param = [{a,b}, {c,d}, {e,f}],
    Param2 = [{a,b}, {e,f}],
    Forms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param},
        {attribute,3,malias,Param2}
    ],
    Description1 = io_lib:format("Element ~p is duplicated on line ~p", [{a,b}, 2]),
    Description2 = io_lib:format("Element ~p is duplicated on line ~p", [{e,f}, 2]),
    ExpectedEForms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param},
        {attribute,3,malias,Param2},
        {warning,{3,malias,Description1}},
        {warning,{3,malias,Description2}}
    ],
    EForms = malias:parse_transform(Forms, []),
    ?assertMatch(ExpectedEForms, EForms).

ab_ac_same_list_test() ->
    Param = [{a,b}, {e,f}, {a,c}],
    Forms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param}
    ],
    Description = io_lib:format("Module ~p aliased to several modules: ~p, ~p", [a,b,c]),
    ExpectedEForms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param},
        {error,{2,malias,Description}}
    ],
    EForms = malias:parse_transform(Forms, []),
    ?assertMatch(ExpectedEForms, EForms).

ab_ac_crosslist_test() ->
    Param1 = [{d,h}, {e,f}, {a,b}],
    Param2 = [{a,c}],
    Param3 = [{a,g}],
    Forms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param1},
        {attribute,3,malias,Param2},
        {attribute,4,malias,Param3}
    ],
    Description1 = io_lib:format("Module ~p aliased to several modules: ~p, ~p on lines: ~p, ~p", [a,b,c,2,3]),
    Description2 = io_lib:format("Module ~p aliased to several modules: ~p, ~p on lines: ~p, ~p", [a,b,g,2,4]),
    Description3 = io_lib:format("Module ~p aliased to several modules: ~p, ~p on lines: ~p, ~p", [a,c,g,3,4]),
    ExpectedEForms = [
        {attribute,1,file,{"test/malias_tests.erl",1}},
        {attribute,1,module,malias_tests},
        {attribute,2,malias,Param1},
        {attribute,3,malias,Param2},
        {error,{3,malias,Description1}},
        {attribute,4,malias,Param3},
        {error,{4,malias,Description3}},
        {error,{4,malias,Description2}}
    ],
    EForms = malias:parse_transform(Forms, []),
    ?assertMatch(ExpectedEForms, EForms).

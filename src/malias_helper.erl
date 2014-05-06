-module(malias_helper).

-export([process_malias_options/1, cleanup/0, format_error/1, change_module_name/1]).

-record(state, {error=false, pairs=[]}).

-define(PTMODULE, malias).

%TODO:
% * Add example of Emake-file
% * describe tuple-usage in doc

process_malias_options(Forms) ->
    case fetch_pairs(Forms) of
        {error, EForms} ->
            {ignore, EForms};
        {ok, [], Forms2} ->
            % skip malias processing, when no options found
            {ignore, Forms2};
        {ok, Pairs, Forms2} ->
            erlang:put(malias_opts, Pairs),
            {process, Forms2}
    end.

cleanup() ->
    erlang:erase(malias_opts).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

change_module_name(M0={atom,Line,Module}) ->
    Changes = erlang:get(malias_opts),
    case proplists:lookup(Module, Changes) of
        {Module, Original} ->
            {atom,Line,Original};
        none ->
            M0
    end;
change_module_name(M0) ->
    M0.


%%% private

fetch_pairs(Forms) ->
    {Forms2, State} = walk_forms(Forms),
    Pairs = transform_pairs(State#state.pairs),
    case State#state.error of
        true  -> {error, Forms2};
        false -> {ok, Pairs, Forms2}
    end.

walk_forms(Forms) ->
    {RevForms2, State} = walk_forms(Forms, [], #state{}),
    Forms2 = lists:reverse(RevForms2),
    {Forms2, State}.

walk_forms([], Acc, State) ->
    {Acc, State};
walk_forms([F|Forms], Acc, State) ->
    {Items, State2} = handle_malias(F, State),
    walk_forms(Forms, Items ++ Acc, State2).

transform_pairs(List) when is_list(List) ->
    Fun = fun({_,A,B}) -> {B,A} end,
    lists:map(Fun, List).

handle_malias(F={attribute, Line, malias, {A,B}}, S) when is_atom(A), is_atom(B) ->
    Pairs = [{Line, A, B}],
    handle_items(F, Pairs, S);
% ignore empty list
handle_malias(F={attribute, _Line, malias, []}, S) ->
    {[F], S};
handle_malias(F={attribute, Line, malias, List}, S) when is_list(List) ->
    case correct_list(List) of
        true ->
            Fun = fun({A,B}) -> {Line,A,B} end,
            Pairs = lists:map(Fun, List),
            handle_items(F, Pairs, S);
            %{[F], S#state{pairs=Pairs2}};
        false ->
            Error = parameter_error(Line, List),
            {[Error], S#state{error=true}}
    end;
handle_malias({attribute, Line, malias, Term}, S) ->
    Error = parameter_error(Line, Term),
    {[Error], S#state{error=true}};
handle_malias(F, S) ->
    {[F], S}.

handle_items(Form, Pairs, S) ->
    OldPairs = S#state.pairs,
    WForms = lookup_ab_ab_same_list(Pairs) ++ lookup_ab_ab_cross_lists(Pairs, OldPairs),
    EForms = lookup_aa_same_list(Pairs) ++ lookup_ab_ac_same_list(Pairs) ++ lookup_ab_ac_cross_lists(Pairs, OldPairs) ++ lookup_ab_cb_same_list(Pairs) ++ lookup_ab_cb_cross_lists(Pairs, OldPairs) ++ lookup_ab_bc_same_list(Pairs) ++ lookup_ab_bc_cross_lists(Pairs, OldPairs),
    Pairs2 = Pairs ++ OldPairs,
    {EForms ++ WForms ++ [Form], S#state{pairs=Pairs2}}.

lookup_aa_same_list(List) when is_list(List) ->
    Line = element(1, hd(List)),
    case lists:filter(fun matcher_aa/1, List) of
        [] ->
            [];
        L when is_list(L)  ->
            UniquePairs = unique_pairs(L),
            [aa_error(Line, Pair) || Pair <- UniquePairs]
    end.

lookup_ab_ab_same_list(List) when is_list(List) ->
    lookup_t1_t2_same_list(ab_ab, fun matcher_ab_ab/1, fun ab_ab_warning/2, List).

lookup_ab_ac_same_list(List) when is_list(List) ->
    lookup_t1_t2_same_list(ab_ac, fun matcher_ab_ac/1, fun ab_ac_error/2, List).

lookup_ab_cb_same_list(List) when is_list(List) ->
    lookup_t1_t2_same_list(ab_cb, fun matcher_ab_cb/1, fun ab_cb_error/2, List).

lookup_ab_bc_same_list(List) when is_list(List) ->
    lookup_t1_t2_same_list(ab_bc, fun matcher_ab_bc/1, fun ab_bc_error/2, List).

lookup_t1_t2_same_list(Kind, Matcher, ErrorFun, List) when is_list(List) ->
    Line = element(1, hd(List)),
    case iterate_with_tail(List, Matcher, []) of
        [] ->
            [];
        L when is_list(L)  ->
            UniquePairs = unique_pairs(L),
            case Kind of
                ab_ab ->
                    [ab_ab_warning(Line, UniquePairs)];
                _ ->
                    [ErrorFun(Line, Pair) || Pair <- lists:reverse(UniquePairs)]
            end
    end.

lookup_ab_ab_cross_lists(Pairs, OldPairs) ->
    lookup_t1_t2_cross_lists(fun matcher_ab_ab/1, fun ab_ab_cross_warning/2, Pairs, OldPairs).

lookup_ab_ac_cross_lists(Pairs, OldPairs) when is_list(Pairs) ->
    lookup_t1_t2_cross_lists(fun matcher_ab_ac/1, fun ab_ac_cross_error/2, Pairs, OldPairs).

lookup_ab_cb_cross_lists(Pairs, OldPairs) when is_list(Pairs) ->
    lookup_t1_t2_cross_lists(fun matcher_ab_cb/1, fun ab_cb_cross_error/2, Pairs, OldPairs).

lookup_ab_bc_cross_lists(Pairs, OldPairs) when is_list(Pairs) ->
    lookup_t1_t2_cross_lists(fun matcher_ab_bc/1, fun ab_bc_cross_error/2, Pairs, OldPairs).

lookup_t1_t2_cross_lists(Matcher, ErrorFun, Pairs, OldPairs) ->
    Line = element(1, hd(Pairs)),
    UniqPairs = unique_pairs(Pairs),
    Duplicates = iterate_with_list(UniqPairs, OldPairs, Matcher),
    [ErrorFun(Line, Tuples) || Tuples <- lists:reverse(Duplicates)].

iterate_with_list(List, List2, Matcher) ->
    Fun = fun(T, Acc) ->
            match_all(T, List2, Matcher) ++ Acc
    end,
    lists:foldl(Fun, [], List).

iterate_with_tail([], _Matcher, Acc) ->
    Acc;
iterate_with_tail([H|T], Matcher, Acc) ->
    Elements = match_all(H, T, Matcher),
    iterate_with_tail(T, Matcher, Elements ++ Acc).

match_all(H, T, Matcher) ->
    MatcherH = Matcher(H),
    Fun = fun(X) ->
            case MatcherH(X) of
                true -> {true, {H,X}};
                false -> false
            end
    end,
    lists:filtermap(Fun, T).

matcher_aa({_,A,B}) ->
    A =:= B.

matcher_ab_ab({_,A,B}) ->
    fun({_,X,Y}) -> X =:= A andalso Y =:= B end.

matcher_ab_ac({_,A,B}) ->
    fun({_,X,Y}) -> X =:= A andalso Y =/= B end.

matcher_ab_cb({_,A,B}) ->
    fun({_,X,Y}) -> X =/= A andalso Y =:= B end.

matcher_ab_bc({_,A,B}) ->
    fun({_,X,Y}) ->
            (X =:= B andalso Y =/= A) orelse
            (X =/= B andalso Y =:= A)
    end.

unique_pairs(L) when is_list(L) ->
    unique_pairs(L, []).

unique_pairs([], Acc) ->
    Acc;
unique_pairs([H|T], Acc) ->
    case is_member(H, T) of
        true  -> unique_pairs(T, Acc);
        false -> unique_pairs(T, [H|Acc])
    end.

is_member({{_,A,B}, {_,C,D}}, T) when is_list(T) ->
    Matcher = fun({{_,W,Z},{_,X,Y}}) ->
            not(W =:= A andalso Z =:= B andalso X =:= C andalso Y =:= D)
    end,
    case lists:dropwhile(Matcher, T) of
        [] -> false;
        _ -> true
    end;
is_member({_,A,B}, T) when is_list(T) ->
    Matcher = fun({_,X,Y}) -> X =/= A orelse Y =/= B end,
    case lists:dropwhile(Matcher, T) of
        [] -> false;
        _ -> true
    end.

correct_list(List) when is_list(List) ->
    Fun = fun({A,B}) when is_atom(A), is_atom(B) -> true;
        (_) -> false
    end,
    lists:all(Fun, List).

parameter_error(Line, Term) ->
    Description = io_lib:format("Incorrect parameter for malias: ~p~n", [Term]),
    {error, {Line, ?PTMODULE, Description}}.

aa_error(Line, {_,A,A}) ->
    Description = io_lib:format("Module ~p is aliased to itself", [A]),
    io:format(Description),
    {error, {Line, ?PTMODULE, Description}}.

ab_ac_error(Line, {{_,A,B},{_,A,C}}) ->
    Description = io_lib:format("Module ~p aliased to several modules: ~p, ~p", [A,B,C]),
    {error, {Line, ?PTMODULE, Description}}.

ab_cb_error(Line, {{_,A,B},{_,C,B}}) ->
    Description = io_lib:format("Modules ~p and ~p aliased to the same module ~p", [A,C,B]),
    {error, {Line, ?PTMODULE, Description}}.

ab_bc_error(Line, {{L2,B,C},{L1,A,B}}) ->
    ab_bc_error(Line, {{L1,A,B},{L2,B,C}});
ab_bc_error(Line, {{_,A,B},{_,B,C}}) ->
    Description = io_lib:format("Cross aliasing error. Module ~p is aliased to ~p and module ~p is aliased to ~p", [A,B,B,C]),
    {error, {Line, ?PTMODULE, Description}}.

ab_ac_cross_error(Line, {{CLine,A,C},{BLine,A,B}}) ->
    Description = io_lib:format("Module ~p aliased to several modules: ~p, ~p on lines: ~p, ~p", [A,B,C,BLine,CLine]),
    {error, {Line, ?PTMODULE, Description}}.

ab_cb_cross_error(Line, {{ALine,A,B},{CLine,C,B}}) ->
    Description = io_lib:format("Modules ~p and ~p aliased to the same module ~p on lines: ~p, ~p", [A,C,B,ALine,CLine]),
    {error, {Line, ?PTMODULE, Description}}.

ab_bc_cross_error(Line, {{BLine,B,C}, {ALine,A,B}}) ->
    ab_bc_cross_error(Line, {{ALine,A,B},{BLine,B,C}});
ab_bc_cross_error(Line, {{ALine,A,B},{BLine,B,C}}) ->
    Description = io_lib:format("Cross aliasing error. Module ~p is aliased to ~p and module ~p is aliased to ~p on lines: ~p, ~p", [A,B,B,C,ALine,BLine]),
    {error, {Line, ?PTMODULE, Description}}.

ab_ab_warning(Line, Tuples) ->
    ABList = lists:map(fun({{_,A,B},_T2}) -> {A,B} end, Tuples),
    Format = string:join(lists:duplicate(length(Tuples), "~p"), ", "),
    Description = io_lib:format("Elements are duplicated: " ++ Format, ABList),
    {warning, {Line, ?PTMODULE, Description}}.

ab_ab_cross_warning(Line, {{_,A,B},{PrevLine,A,B}}) ->
    Description = io_lib:format("Element ~p is duplicated on line ~p", [{A,B}, PrevLine]),
    {warning, {Line, ?PTMODULE, Description}}.

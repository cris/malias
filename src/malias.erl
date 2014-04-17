-module(malias).

-export([parse_transform/2, format_error/1]).

-record(state, {error=false, pairs=[]}).

%TODO:
% * warn in case of duplicated import
% * error in case of overwritten import, e.g. [{io, one}, {cool, one}]
%  - show on which lines such conflict occurs
%    (line 7, line 9 for 2 malias options)
% * Add example of Emake-file
% * describe tuple-usage in doc

parse_transform(Forms, _Options) ->
    %io:format("here ~p~n", [Forms]),
    case fetch_pairs(Forms) of
        {error, EForms} ->
            %io:format("here ~p~n", [EForms]),
            EForms;
        {ok, Pairs, Forms2} ->
            transform(Pairs, Forms2)
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

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

handle_malias(F={attribute, Line, malias, {A,B}}, S) when is_atom(A), is_atom(B) ->
    Pairs2 = [{Line, A, B} | S#state.pairs],
    {[F], S#state{pairs=Pairs2}};
handle_malias(F={attribute, Line, malias, List}, S) when is_list(List) ->
    case correct_list(List) of
        true ->
            Fun = fun({A,B}) -> {Line,A,B} end,
            ListWithLine = lists:map(Fun, List),
            Pairs2 = ListWithLine ++ S#state.pairs,
            {[F], S#state{pairs=Pairs2}};
        false ->
            Error = parameter_error(Line, List),
            {[Error], S#state{error=true}}
    end;
handle_malias({attribute, Line, malias, Term}, S) ->
    Error = parameter_error(Line, Term),
    {[Error], S#state{error=true}};
handle_malias(F, S) ->
    {[F], S}.

correct_list(List) when is_list(List) ->
    Fun = fun({A,B}) when is_atom(A), is_atom(B) -> true;
        (_) -> false
    end,
    lists:all(Fun, List).

parameter_error(Line, Term) ->
    Description = io_lib:format("Incorrect parameter for malias: ~p~n", [Term]),
    {error, {Line, ?MODULE, Description}}.

transform_pairs(List) when is_list(List) ->
    Fun = fun({_,A,B}) -> {B,A} end,
    lists:map(Fun, List).

transform([], Forms) ->
    Forms;
transform(Changes, Forms) ->
    Tree = erl_syntax:form_list(Forms),
    ModifiedTree = postorder(Changes, Tree),
    Forms2 = erl_syntax:revert_forms(ModifiedTree),
    %io:format("Result tree~n~p~n", [Forms2]),
    Forms2.

apply_malias(Changes, Node) ->
    case erl_syntax:type(Node) of
        module_qualifier ->
            Argument = erl_syntax:module_qualifier_argument(Node),
            Module = erl_syntax:atom_value(Argument),
            case proplists:lookup(Module, Changes) of
                {Module, Original} ->
                    Body = erl_syntax:module_qualifier_body(Node),
                    NewArgument = erl_syntax:atom(Original),
                    erl_syntax:module_qualifier(NewArgument, Body);
                none ->
                    Node
            end;
        _ ->
            Node
    end.

postorder(Changes, Tree) ->
    apply_malias(Changes, case erl_syntax:subtrees(Tree) of
            [] -> Tree;
            List -> erl_syntax:update_tree(Tree,
                    [[postorder(Changes, Subtree)
                            || Subtree <- Group]
                        || Group <- List])
        end).

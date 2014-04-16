-module(malias).

-export([parse_transform/2]).

%TODO:
% * check correct malias list:
%  - should contains only {atom, atom}
%  - or only one tuple {atom, atom}
% * warn in case of duplicated import
% * error in case of overwritten import, e.g. [{io, one}, {cool, one}]
%  - show on which lines such conflict occurs
%    (line 7, line 9 for 2 malias options)

transform(Changes, Forms) ->
    Tree = erl_syntax:form_list(Forms),
    ModifiedTree = postorder(Changes, Tree),
    Forms2 = erl_syntax:revert_forms(ModifiedTree),
    %io:format("Result tree~n~p~n", [Forms2]),
    Forms2.

parse_transform(Forms, _Options) ->
    PList = fetch_substitutions(Forms),
    transform(PList, Forms).

fetch_substitutions(Forms) ->
    Changes = lists:filtermap(fun is_malias/1, Forms),
    FChanges = lists:flatten(Changes),
    lists:map(fun swap_tuple/1, FChanges).

is_malias({attribute, _Line, malias, List}) ->
    {true, List};
is_malias(_) ->
    false.

swap_tuple({A,B}) ->
    {B,A}.


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

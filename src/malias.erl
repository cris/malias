%% This file is based on a copy of erl_id_trans.erl from the OTP 17.0.1
%% Erlang/OTP

%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(malias).

%% A identity transformer of Erlang abstract syntax.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for tranforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2, format_error/1]).

-record(state, {error=false, pairs=[]}).

-define(PTMODULE, malias).

parse_transform(Forms, _Options) ->
    Forms3 = case process_malias_options(Forms) of
        {process, Forms2} ->
            forms(Forms2);
        {ignore, Forms2} ->
            Forms2
    end,
    malias_cleanup(),
    Forms3.

format_error(Message) ->
    malias_format_error(Message).

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0]) ->
    F1 = form(F0),
    Fs1 = forms(Fs0),
    [F1|Fs1];
forms([]) -> [].

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
form({attribute,Line,module,Mod}) ->
    {attribute,Line,module,Mod};
form({attribute,Line,file,{File,Line}}) ->	%This is valid anywhere.
    {attribute,Line,file,{File,Line}};
form({attribute,Line,export,Es0}) ->
    Es1 = farity_list(Es0),
    {attribute,Line,export,Es1};
form({attribute,Line,import,{Mod,Is0}}) ->
    Is1 = farity_list(Is0),
    {attribute,Line,import,{Mod,Is1}};
form({attribute,Line,compile,C}) ->
    {attribute,Line,compile,C};
form({attribute,Line,record,{Name,Defs0}}) ->
    Defs1 = record_defs(Defs0),
    {attribute,Line,record,{Name,Defs1}};
form({attribute,Line,asm,{function,N,A,Code}}) ->
    {attribute,Line,asm,{function,N,A,Code}};
form({attribute,Line,Attr,Val}) ->		%The general attribute.
    {attribute,Line,Attr,Val};
form({function,Line,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Line,Name,Arity,Clauses};
% Mnemosyne, ignore...
form({rule,Line,Name,Arity,Body}) ->
    {rule,Line,Name,Arity,Body}; % Dont dig into this
%% Extra forms from the parser.
form({error,E}) -> {error,E};
form({warning,W}) -> {warning,W};
form({eof,Line}) -> {eof,Line}.

%% -type farity_list([Farity]) -> [Farity] when Farity <= {atom(),integer()}.

farity_list([{Name,Arity}|Fas]) ->
    [{Name,Arity}|farity_list(Fas)];
farity_list([]) -> [].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs([{record_field,Line,{atom,La,A},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Line,{atom,La,A},Val1}|record_defs(Is)];
record_defs([{record_field,Line,{atom,La,A}}|Is]) ->
    [{record_field,Line,{atom,La,A}}|record_defs(Is)];
record_defs([]) -> [].

%% -type function(atom(), integer(), [Clause]) -> {atom(),integer(),[Clause]}.

function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name,Arity,Clauses1}.

%% -type clauses([Clause]) -> [Clause].

clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].

%% -type clause(Clause) -> Clause.

clause({clause,Line,H0,G0,B0}) ->
    H1 = head(H0),
    G1 = guard(G0),
    B1 = exprs(B0),
    {clause,Line,H1,G1,B1}.

%% -type head([Pattern]) -> [Pattern].

head(Ps) -> patterns(Ps).

%% -type patterns([Pattern]) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|patterns(Ps)];
patterns([]) -> [].

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}) -> {var,Line,V};
pattern({match,Line,L0,R0}) ->
    L1 = pattern(L0),
    R1 = pattern(R0),
    {match,Line,L1,R1};
pattern({integer,Line,I}) -> {integer,Line,I};
pattern({char,Line,C}) -> {char,Line,C};
pattern({float,Line,F}) -> {float,Line,F};
pattern({atom,Line,A}) -> {atom,Line,A};
pattern({string,Line,S}) -> {string,Line,S};
pattern({nil,Line}) -> {nil,Line};
pattern({cons,Line,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,Line,Ps1};
pattern({map,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {map,Line,Ps1};
pattern({map_field_exact,Line,K,V}) ->
    Ke = expr(K),
    Ve = pattern(V),
    {map_field_exact,Line,Ke,Ve};
%%pattern({struct,Line,Tag,Ps0}) ->
%%    Ps1 = pattern_list(Ps0),
%%    {struct,Line,Tag,Ps1};
pattern({record,Line,Name,Pfs0}) ->
    Pfs1 = pattern_fields(Pfs0),
    {record,Line,Name,Pfs1};
pattern({record_index,Line,Name,Field0}) ->
    Field1 = pattern(Field0),
    {record_index,Line,Name,Field1};
pattern({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
pattern({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
pattern({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
pattern({op,Line,Op,A}) ->
    {op,Line,Op,A};
pattern({op,Line,Op,L,R}) ->
    {op,Line,Op,L,R}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs]) ->
    S2 = case S1 of
	     default ->
		 default;
	     _ ->
		 expr(S1)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1)
	 end,
    [{bin_element,L1,expr(E1),S2,T2} | pattern_grp(Fs)];
pattern_grp([]) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].



%% -type pattern_list([Pattern]) -> [Pattern].
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|pattern_list(Ps)];
pattern_list([]) -> [].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{atom,La,F},P1}|pattern_fields(Pfs)];
pattern_fields([{record_field,Lf,{var,La,'_'},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{var,La,'_'},P1}|pattern_fields(Pfs)];
pattern_fields([]) -> [].

%% -type guard([GuardTest]) -> [GuardTest].

guard([G0|Gs]) when is_list(G0) ->
    [guard0(G0) | guard(Gs)];
guard(L) ->
    guard0(L).

guard0([G0|Gs]) ->
    G1 =  guard_test(G0),
    [G1|guard0(Gs)];
guard0([]) -> [].

guard_test(Expr={call,Line,{atom,La,F},As0}) ->
    case erl_internal:type_test(F, length(As0)) of
	true -> 
	    As1 = gexpr_list(As0),
	    {call,Line,{atom,La,F},As1};
	_ ->
	    gexpr(Expr)
    end;
guard_test(Any) ->
    gexpr(Any).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr) -> GuardExpr.

gexpr({var,Line,V}) -> {var,Line,V};
gexpr({integer,Line,I}) -> {integer,Line,I};
gexpr({char,Line,C}) -> {char,Line,C};
gexpr({float,Line,F}) -> {float,Line,F};
gexpr({atom,Line,A}) -> {atom,Line,A};
gexpr({string,Line,S}) -> {string,Line,S};
gexpr({nil,Line}) -> {nil,Line};
gexpr({map,Line,Map0,Es0}) ->
    [Map1|Es1] = gexpr_list([Map0|Es0]),
    {map,Line,Map1,Es1};
gexpr({map,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {map,Line,Es1};
gexpr({map_field_assoc,Line,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_assoc,Line,Ke,Ve};
gexpr({map_field_exact,Line,K,V}) ->
    Ke = gexpr(K),
    Ve = gexpr(V),
    {map_field_exact,Line,Ke,Ve};
gexpr({cons,Line,H0,T0}) ->
    H1 = gexpr(H0),
    T1 = gexpr(T0),				%They see the same variables
    {cons,Line,H1,T1};
gexpr({tuple,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,Line,Es1};
gexpr({record_index,Line,Name,Field0}) ->
    Field1 = gexpr(Field0),
    {record_index,Line,Name,Field1};
gexpr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = gexpr(Rec0),
    Field1 = gexpr(Field0),
    {record_field,Line,Rec1,Name,Field1};
gexpr({record,Line,Name,Inits0}) ->
    Inits1 = grecord_inits(Inits0),
    {record,Line,Name,Inits1};
gexpr({call,Line,{atom,La,F},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Line,{atom,La,F},As1}
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or 
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As1}
    end;
gexpr({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
gexpr({op,Line,Op,A0}) ->
    case erl_internal:arith_op(Op, 1) or 
	 erl_internal:bool_op(Op, 1) of
	true -> A1 = gexpr(A0),
		{op,Line,Op,A1}
    end;
gexpr({op,Line,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {op,Line,Op,L1,R1};
gexpr({op,Line,Op,L0,R0}) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or 
	  erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0),
	    R1 = gexpr(R0),			%They see the same variables
	    {op,Line,Op,L1,R1}
    end.

%% -type gexpr_list([GuardExpr]) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

grecord_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|grecord_inits(Is)];
grecord_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|grecord_inits(Is)];
grecord_inits([]) -> [].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

%% -type expr(Expression) -> Expression.

expr({var,Line,V}) -> {var,Line,V};
expr({integer,Line,I}) -> {integer,Line,I};
expr({float,Line,F}) -> {float,Line,F};
expr({atom,Line,A}) -> {atom,Line,A};
expr({string,Line,S}) -> {string,Line,S};
expr({char,Line,C}) -> {char,Line,C};
expr({nil,Line}) -> {nil,Line};
expr({cons,Line,H0,T0}) ->
    H1 = expr(H0),
    T1 = expr(T0),				%They see the same variables
    {cons,Line,H1,T1};
expr({lc,Line,E0,Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {lc,Line,E1,Qs1};
expr({bc,Line,E0,Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {bc,Line,E1,Qs1};
expr({tuple,Line,Es0}) ->
    Es1 = expr_list(Es0),
    {tuple,Line,Es1};
expr({map,Line,Map0,Es0}) ->
    [Map1|Es1] = exprs([Map0|Es0]),
    {map,Line,Map1,Es1};
expr({map,Line,Es0}) ->
    Es1 = exprs(Es0),
    {map,Line,Es1};
expr({map_field_assoc,Line,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_assoc,Line,Ke,Ve};
expr({map_field_exact,Line,K,V}) ->
    Ke = expr(K),
    Ve = expr(V),
    {map_field_exact,Line,Ke,Ve};
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = pattern_list(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index,Line,Name,Field0}) ->
    Field1 = expr(Field0),
    {record_index,Line,Name,Field1};
expr({record,Line,Name,Inits0}) ->
    Inits1 = record_inits(Inits0),
    {record,Line,Name,Inits1};
expr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
expr({record,Line,Rec0,Name,Upds0}) ->
    Rec1 = expr(Rec0),
    Upds1 = record_updates(Upds0),
    {record,Line,Rec1,Name,Upds1};
expr({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
expr({block,Line,Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block,Line,Es1};
expr({'if',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'try',Line,Es0,Scs0,Ccs0,As0}) ->
    Es1 = exprs(Es0),
    Scs1 = icr_clauses(Scs0),
    Ccs1 = icr_clauses(Ccs0),
    As1 = exprs(As0),
    {'try',Line,Es1,Scs1,Ccs1,As1};
expr({'fun',Line,Body}) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0),
	    {'fun',Line,{clauses,Cs1}};
	{function,F,A} ->
	    {'fun',Line,{function,F,A}};
	{function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
	    %% R10B-6: fun M:F/A. (Backward compatibility)
	    {'fun',Line,{function,M,F,A}};
	{function,M0,F0,A0} ->
	    %% R15: fun M:F/A with variables.
        M = change_module_name(M0),
	    F = expr(F0),
	    A = expr(A0),
	    {'fun',Line,{function,M,F,A}}
    end;
expr({named_fun,Loc,Name,Cs}) ->
    {named_fun,Loc,Name,fun_clauses(Cs)};
expr({call,Line,F0,As0}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0),
    As1 = expr_list(As0),
    {call,Line,F1,As1};
expr({'catch',Line,E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch',Line,E1};
expr({match,Line,P0,E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match,Line,P1,E1};
expr({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
expr({op,Line,Op,A0}) ->
    A1 = expr(A0),
    {op,Line,Op,A1};
expr({op,Line,Op,L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0}) ->
    M1 = change_module_name(M0),
    F1 = expr(F0),
    {remote,Line,M1,F1}.

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0),
    [E1|expr_list(Es)];
expr_list([]) -> [].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_inits(Is)];
record_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|record_inits(Is)];
record_inits([]) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field,Lf,{atom,La,F},Val0}|Us]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_updates(Us)];
record_updates([]) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|icr_clauses(Cs)];
icr_clauses([]) -> [].

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([{b_generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{b_generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([E0|Qs]) ->
    E1 = expr(E0),
    [E1|lc_bc_quals(Qs)];
lc_bc_quals([]) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|fun_clauses(Cs)];
fun_clauses([]) -> [].


%% Malias helper part
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

malias_cleanup() ->
    erlang:erase(malias_opts).

malias_format_error(Message) ->
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

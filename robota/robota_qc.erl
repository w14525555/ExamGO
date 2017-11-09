%% @author wuyou
%% @doc @todo Add description to robota_qc.

-include_lib("eqc/include/eqc.hrl").

-module(robota_qc).

%% ====================================================================
%% API functions
%% ====================================================================
-export([grader/0, assignment/0, prop_delete/0]).

%% Generator of Grader
grader()->
    undefined.

assignment()->
    undefined.

prop_delete() ->
    ?FORALL({X, Xs}, {int(), list(int())},
    ?IMPLIES(lists:member(X, Xs),
    not lists:member(X, lists:delete(X, Xs)))).

%% ====================================================================
%% Internal functions
%% ====================================================================



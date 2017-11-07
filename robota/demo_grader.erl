%% @author wuyou
%% @doc @todo Add description to modularGraderExample.


-module(demo_grader).


%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/1, grade/2, unload/1]).


setup(Init) ->
    case Init of
        fail_test ->
            {error, for_error_test};
        _Else ->
            {ok, Init/2}
	end.

grade(State, {_, Answer}) ->
    {ok, case (Answer + State) of
             0.0 -> looks_good;
             _ -> failed
         end}.

unload(_State) ->
    ok.




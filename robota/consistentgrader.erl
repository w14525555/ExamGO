-module(consistentgrader).

-export([setup/1, grade/2, unload/1]).

setup(_) ->
    Now = erlang:system_time(microsecond),
    Bin = term_to_binary(Now),
    <<Salt:8, _/binary>> = erlang:md5(Bin),
    {ok, Salt}.

grade(Salt, {_, Answer}) ->
    Bin = term_to_binary(Answer),
    <<Desider:8, _/binary>> = erlang:md5(Bin),
    {ok, case (Desider + Salt) rem 2 of
             0 -> looks_good;
             _ -> failed
         end}.

unload(_Salt) ->
    ok.

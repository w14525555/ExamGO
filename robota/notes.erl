%% @author wuyou
%% @doc @todo Add description to notes.


-module(notes).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).



%% ====================================================================
%% Internal functions
%% ====================================================================

%% Assignment: 
%% List of parts:  [{Label, Grader}, ..., ] 
%% Label is atom, Grader is a grader. All labels are different.
%% Assignment Can be Avaliable or unavailable

%% Submission:
%% List of answers: [{Label, Answer}, ..., ]
%% Label is an atom, answer is an term
%% Valid if labels in submissions exits
%% Partial if not contain all parts

%% Feedback:
%% List of labeled result: [{Label, Result}, ..., ]
%% For partial, the result should be atom missing. 

%% Grader:
%% Get a labeled answer and give a result of analysis
%% Simple Grader and Composed Grader
%% Forgiveing: abraham -> {Label, looks_good}
%% Angry: niels -> {Label, failed}
%% Matching: {mikkel, Expect} ->  {Label, failed} or {Label, looks_good}
%% Testing Grader, answer is a function : {simon, Arg, Expect} -> Answer(Arg) =? Expecct ->  {Label, failed} or {Label, looks_good}
%% Modular Grader: {andrzej, Callback, Init} -> Callback Init(Arg) -> Grade() ->  {Label, failed} or {Label, looks_good}
%% Cocurrent Grader: {troels, Gs} -> GS: [Grader, ...] use them in order -> Each spawn a process -> If one fails, all other exit should be failed
%% Fail: non-normal fasion or longer than 3 seconds  -> {Label, grader_failed}

%% Grader Module: 
%% setup(Init) -> {ok, State} | {error, Reason}
%% grade(State, LAnswer) -> State is return value of setup, LAnswer is Labeled Answer -> {ok, Result} | {error, Reason}
%% upload(State) -> stop all the process start by module -> ok | {error, Reason}

%% robata
%%
%% @author wuyou
%% @doc @todo Add description to demo.

%% Run the demo
%% demo:run().

-module(demo).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, run/0, grade_in_student_process/3]).


run()->
	AssignmentName = "Final Exam",
    RoboTa = start(),
	AssHandler = new(RoboTa, AssignmentName),
    add_all_graders(AssHandler),
    status_before_available(AssHandler),
    available(AssHandler),
    status_after_available(AssHandler),
    sending_submission_in_student_process(RoboTa, AssignmentName),
    unavailable(AssHandler),
    status_after_unavailable(AssHandler),
    io:format("Demo Finishes!\n").

%% Start a RoboTa Server
start()->
    {ok, RoboTa} = robota:get_the_show_started(),
    matches("Test start() returns a RoboTa Server pid", true, is_pid(RoboTa)),
    RoboTa.

%% Create new Assignemnt 
new(RoboTa, AssignmentName)->
	{ok, AssHandler} = robota:new(RoboTa, AssignmentName),
    {AssName, _} = AssHandler,
    matches("Test Ass handler includes the right Assignment Name", AssName, AssignmentName),
	AssHandler.

%% Add all graders
add_all_graders(AssHandler)->
    robota:add_part(AssHandler, abraham_grader, abraham),
    robota:add_part(AssHandler, niels_grader, niels),
    robota:add_part(AssHandler, mikkel_grader, {mikkel, 6}),
    robota:add_part(AssHandler, simon_grader, {simon, 8, 16}),
    robota:add_part(AssHandler, modular_grader, {andrzej, demo_grader, 2}),
    robota:add_part(AssHandler, concurrent_grader, {troels, [abraham, niels, {mikkel, match}, {simon, 2, 3}, {andrzej, demo_grader, 6}]}).

%% Test status before available
status_before_available(AssHandler)->
    {Status, Parts} = robota:status(AssHandler),
    ExpectedParts = [{abraham_grader, abraham}, {niels_grader, niels}, {mikkel_grader, {mikkel,6}},{simon_grader,
                    {simon,8,16}},{modular_grader,{andrzej,demo_grader,2}},{concurrent_grader,
                    {troels,[abraham,niels,{mikkel,match},{simon,2,3},{andrzej,demo_grader,6}]}}],
    matches("Test status is unavailable before teacher process make it available", unavailable, Status),
    matches("Test status() function return the right Parts", ExpectedParts, Parts).

%% Test status after making it available
status_after_available(AssHandler)->
        {Status, Parts} = robota:status(AssHandler),
        ExpectedParts = [{abraham_grader, abraham}, {niels_grader, niels}, {mikkel_grader, {mikkel,6}},{simon_grader,
                        {simon,8,16}},{modular_grader,{andrzej,demo_grader,2, 1.0}},{concurrent_grader,
                        {troels,[abraham,niels,{mikkel,match},{simon,2,3},{andrzej,demo_grader,6, 3.0}]}}],
        matches("Test status is available after teacher process make it available", available, Status),
        matches("Test status() function return the right Parts after availlable", ExpectedParts, Parts).

available(AssHandler)->
    Success = robota:available(AssHandler),
    matches("Test available returns ok", ok, Success).

sending_submission_in_student_process(RoboTa, AssignmentName)->
    ExprectedResult = [{abraham_grader, looks_good}, {niels_grader, failed}, {mikkel_grader, looks_good},
  	                    {simon_grader, looks_good}, {modular_grader, looks_good},
                       {concurrent_grader, [looks_good, failed, looks_good, looks_good, failed]}],
    spawn(demo, grade_in_student_process, [RoboTa, AssignmentName, self()]),
		receive
			{ok, Result} ->
				matches("Test grade() returns expected Reuslt", ExprectedResult, Result);
			{error, Reason} ->
				matches("Test grade() return right error message", error, Reason)
		end.

grade_in_student_process(RoboTa, AssignmentName, TeacherProcess)->
    Submission = [{abraham_grader, dont_care}, {niels_grader, angry}, {mikkel_grader, 6},
        {simon_grader,fun(A)->A*2 end}, {modular_grader, -1},
        {concurrent_grader, [good, fail, match, fun(A)-> A+1 end, 2]}],
    Ref = robota:grade(RoboTa, AssignmentName, Submission, self()),
        receive
            {Ref, {ok, Result}} ->
                TeacherProcess ! {ok, Result};
            {Ref, {error, Reason}}->
                TeacherProcess ! {error, Reason}
        end.

unavailable(AssHandler) ->
    Success = robota:unavailable(AssHandler),
    matches("Test unavailable returns ok", ok, Success).

%% Test status after making it available
status_after_unavailable(AssHandler)->
        {Status, Parts} = robota:status(AssHandler),
        ExpectedParts = [{abraham_grader, abraham}, {niels_grader, niels}, {mikkel_grader, {mikkel,6}},{simon_grader,
                        {simon,8,16}},{modular_grader,{andrzej,demo_grader,2, 1.0}},{concurrent_grader,
                        {troels,[abraham,niels,{mikkel,match},{simon,2,3},{andrzej,demo_grader,6, 3.0}]}}],
        matches("Test status is unavailable after teacher process make it unavailable", unavailable, Status),
        matches("Test status() function return the right Parts after unavaillable", ExpectedParts, Parts).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% match functions
matches(TestMsg, Expected, Actual) ->
    if 
		Expected =:= Actual -> 
			io:format("~p, passes!\n", [TestMsg]);
        Expected =/= Actual -> 
		   io:format("~p: fails: expected value: ~p, actual value: ~p \n", [TestMsg, Expected, Actual])
    end.

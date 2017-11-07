%% @author wuyou
%% @doc @todo Add description to robotaTest.


-module(robotaTest).

%% ====================================================================
%% Run the Test: 
%%
%% robotaTest:testAll().
%% ====================================================================
-export([testAll/0]).

testAll() -> 
	%% Test start()
	RoboTa = test_show_start(),
	
	%% Test new() 
	%% Case 1 : Success
	Name = "Parser",
	AssHandler = test_new(RoboTa, Name),
	%% Case 2: Duplicate Names
	test_new_with_duplicate_name(RoboTa, Name),
	
	%% Test add_part
	%% Case 1 : Success
	Label = abraham_grader,
	Grader = abraham,
	test_add_part(AssHandler, Label, Grader),
	%% Case 2: Call with bad AssHandler
	test_add_part_with_bad_assHandler(AssHandler, Label, Grader),
	%% Case 3: Call with duplicate Label
	test_add_part_with_duplicate_label(AssHandler, Label, Grader),
	
	%% Test delete_part
	%% Case 1: Success
	%% Use add_part before and after check del_part to check if it works 
	test_del_part(AssHandler, Label, Grader),
	
	%% Test status
	%% Case 1: Test Status before make it available 
	test_status_unavailable(AssHandler),

	%% Test unavailable
	%% Case 1: Test try to make unavailable assignment unavailable
	test_unavailable_fail(AssHandler),

	%% Test Grade
	%% Case 1: Test Grade when assignemnt is unavailable
	test_grade_unavailable(RoboTa, Name),

	%% Befoe We make the assignment available,
	%% We should add all kinds of graders for testing
	add_all_kinds_of_graders(AssHandler),
	
	%% Test Done Message
	%% Test available()
	%% Case 1: Fail because setup function fails
	test_available_fail(AssHandler),
	%% Case 2: Success
	%% After a assignment avaible, we cannot change it anymore
	%% which can be used for testing
	test_available(AssHandler, Label, Grader),
	
	%% Test status
	%% Case 2: Test Status is available
	test_staus_available(AssHandler),

	%% Test Grade
	%% Case 2: Success
	test_grade(RoboTa, Name),
	%% Case 3: Partial Submission
	test_partial_grade(RoboTa, Name),

	%% Test Unavailable
	%% Case 2: unavailable success
	test_unavailable_success(AssHandler),
	
	%% Test Done Message
	io:format("All Tests Are Finished!\n").


%% ====================================================================
%% Internal functions
%% ====================================================================

%% start()
test_show_start()->
    {ok, RoboTa} = robota:get_the_show_started(),
    matches("Test get_show_started() returns a Server pid", true, is_pid(RoboTa)),
	RoboTa.

%% new()
test_new(RoboTa, Name)->
	{ok, AssHandler} = robota:new(RoboTa, Name),
	{AssName, Server} = AssHandler,
	matches("Test new() returns the Assignment Name", Name, AssName),
	matches("Test new() returns the Server Pid", RoboTa, Server),
	AssHandler.

test_new_with_duplicate_name(RoboTa, Name) ->
	{error, Reason} = robota:new(RoboTa, Name),
	matches("Test new() with duplicate name returns right error message", name_already_taken, Reason).

%% add_part()
test_add_part(AssHandler, Label, Grader) ->
	Success = robota:add_part(AssHandler, Label, Grader),
	matches("Test add_part() with right arguments returns ok", ok, Success).

%% Add graders for testing
%% Since we have already have modular grader and forgiving grader
%% We don't have to add it here
add_all_kinds_of_graders(AssHandler) ->
	ok = robota:add_part(AssHandler, niels_grader, niels),
	ok = robota:add_part(AssHandler, mikkel_grader, {mikkel, boom}),
	ok = robota:add_part(AssHandler, simon_grader, {simon, 5, 10}),
	ok = robota:add_part(AssHandler, concurrent_grader, {troels, [abraham, niels, {simon, 2, 3}, {mikkel, 5}, {andrzej, demo_grader, 4}]}).

test_add_part_with_bad_assHandler(AssHandler, Label, Grader) ->
	{_, Server} = AssHandler,
	WrongAssHandler = {"Wrong Name", Server},
	{error, Reason} = robota:add_part(WrongAssHandler, Label, Grader),
	matches("Test add_part() with wrong ass handler returns right error message", bad_ass_handler, Reason).

test_add_part_with_duplicate_label(AssHandler, Label, Grader)->
	{error, Reason} = robota:add_part(AssHandler, Label, Grader),
	{ErrorLabel, ErrorMsg} = Reason,
	matches("Test add_part() with duplicate labels returns right Label", Label, ErrorLabel),
	matches("Test add_part() with duplicate labels returns right error message", not_unique, ErrorMsg).

%% del_part()
%% To Test if it is deleted, just call add_part, if 
%% there is no duplicate Label, the Part is deleted
test_del_part(AssHandler, Label, Grader)->
	%% Before the delete, we cannot add label
	{error, Reason} = robota:add_part(AssHandler, Label, Grader),
	{_, ErrorMsg} = Reason,
	matches("Test before del_part() we cannot add part with the label", not_unique, ErrorMsg),
	%% Now we delete the part
	robota:del_part(AssHandler, Label),
	%% Now the target label is deleted, We can add the label
	Success = robota:add_part(AssHandler, Label, Grader),
	matches("Test after del_part() we can add part with the label", ok, Success).

%% tests for available()
test_available(AssHandler, Label, Grader) ->
	Success = robota:available(AssHandler),
	matches("Test avaliable() success return with ok", ok, Success),
	{error, Reason} = robota:add_part(AssHandler, Label, Grader),
	{_, ErrorMsg} = Reason,
	matches("Test after available add_part() returns {Name, is_available}", is_available, ErrorMsg).

test_available_fail(AssHandler)->
	%% Add a new part with fail setup function
	%% So we cannot make it available
	robota:add_part(AssHandler, fail_grader, {andrzej, demo_grader, fail_test}),
	{error, Reason} = robota:available(AssHandler),
	matches("Test if setup fails available() returns message: setup_fails", setup_fails, Reason),
	%% Delet the part
	robota:del_part(AssHandler, fail_grader),
	%% Add a modular grader for Later use:
	ok = robota:add_part(AssHandler, modular_grader, {andrzej, demo_grader, 2}).

%% Tests for unavailable()
test_unavailable_fail(AssHandler)->
	{error, Reason} = robota:unavailable(AssHandler),
	matches("Test unavailable() returns error if it is arlready unavailable", is_already_unavailable, Reason).

test_unavailable_success(AssHandler)->
	Sucess = robota:unavailable(AssHandler),
	matches("Test unavailable() returns ok on sucess", ok, Sucess).

%% tests for status
test_status_unavailable(AssHandler) ->
	{Status, Parts} = robota:status(AssHandler),
	matches("Test the status() is unavailable before make the assignment available", unavailable, Status),
	ExpectedPartsList = [{abraham_grader, abraham}],
	matches("Test the status() return the right tuple of parts", ExpectedPartsList, Parts).

test_staus_available(AssHandler) ->
	{Status, Parts} = robota:status(AssHandler),
	matches("Test the status() is available after make the assignment available", available, Status),
	ExpectedPartsList = [{abraham_grader,abraham}, {niels_grader,niels},{mikkel_grader, {mikkel,boom}},{simon_grader, {simon,5,10}},
						{concurrent_grader, {troels, [abraham, niels, {simon, 2, 3}, {mikkel, 5}, {andrzej, demo_grader, 4, 2.0}]}},
						{modular_grader, {andrzej, demo_grader, 2, 1.0}}],
	matches("Test the status() return the right tuple including the initial state", ExpectedPartsList, Parts).

%% tests for grade()
test_grade_unavailable(RoboTa, Name) ->
		Ref = robota:grade(RoboTa, Name, none, self()),
		matches("Test grade() return a reference", true, is_reference(Ref)),
			receive
				{Ref, {error, Reason}} ->
					{_, ErrorMsg} = Reason,
					matches("Test grade() returns error if assignment is not available", is_unavailable, ErrorMsg)
			end.

test_grade(RoboTa, Name) ->
	Submission = [{abraham_grader, forgive_answer}, {niels_grader, angry}, {mikkel_grader, boom}, 
				  {simon_grader, fun(A)-> A*2 end}, {concurrent_grader, [forgive, angry, fun(A)->A+1 end, 5,  -1]}, 
				  {modular_grader, -1}],
	Ref = robota:grade(RoboTa, Name, Submission , self()),
	matches("Test grade() returns a reference", true, is_reference(Ref)),
	ExpectedList = [{abraham_grader, looks_good}, {niels_grader, failed}, {mikkel_grader, looks_good},
					{simon_grader, looks_good},{concurrent_grader, [looks_good, failed, looks_good, looks_good, failed]},{modular_grader, looks_good}],
		receive
			{_, {ok, Result}}->
				matches("Test all kinds of grade() (including concurrent grading) returns the right grading result", ExpectedList , Result)
		end.

test_partial_grade(RoboTa, Name) ->
	robota:grade(RoboTa, Name, [], self()),
	ExpectedList = [{abraham_grader,missing}, {niels_grader, missing}, {mikkel_grader, missing},
					{simon_grader, missing}, {concurrent_grader, missing}, {modular_grader ,missing}],
		receive
			{_, {ok, Result}}->
				matches("Test grade() returns missing on partial answers", ExpectedList , Result)
		end.

%% match functions
matches(TestMsg, Expected, Actual) ->
    if 
		Expected =:= Actual -> 
			io:format("~p, passes!\n", [TestMsg]);
        Expected =/= Actual -> 
		   io:format("~p: fails: expected value: ~p, actual value: ~p \n", [TestMsg, Expected, Actual])
    end.


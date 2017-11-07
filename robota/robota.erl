-module(robota).

%% API Required
-export([get_the_show_started/0, new/2,
         add_part/3, del_part/2, available/1, unavailable/1, status/1,
         grade/4]).

%% Utility API
-export([init_all_modular_grader/2, grading_in_new_process/6, handle_modular_grader_in_new_process/5,
         handle_abraham_grader_new_process/2, handle_niels_grader_new_process/2, 
         handle_mikkel_grader_in_new_process/4, handle_simon_grader_in_new_process/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

get_the_show_started() ->
     gen_server:start_link({local, ?SERVER}, ?MODULE, none, []).

%% Blcoking
new(RoboTA, Name) ->
    gen_server:call(RoboTA, {new, Name}).

%% Blcoking
add_part(AssHandler, Label, Grader) ->
    {Name, RoboTA} = AssHandler,
    gen_server:call(RoboTA, {add_part, {Name, Label, Grader}}).

%% Non-blocking
del_part(AssHandler, Label) ->
    {Name, RoboTA} = AssHandler,
    gen_server:cast(RoboTA, {del_part, {Name, Label}}).

%% Blcoking
available(AssHandler) ->
    {Name, RoboTA} = AssHandler,
    gen_server:call(RoboTA, {available, Name}).

%% Blocking
unavailable(AssHandler) ->
    {Name, RoboTA} = AssHandler,
    gen_server:call(RoboTA, {unavailable, Name}).

%% Blocking
status(AssHandler) ->
    {Name, RoboTA} = AssHandler,
    gen_server:call(RoboTA, {status, Name}).

grade(RoboTA, Name, Submission, Pid) ->
    gen_server:call(RoboTA, {grade, {Name, Submission, Pid}}).

%% Gen_server Functions


%%--------------------------------------------------------------------
%% Initializes the server
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%%--------------------------------------------------------------------

%% The Initial State is an Assignment Dictionary.
%% The Second argument is a PID list of the currenting running grading process
%% The third augument is a list of unavailable request waiting for all grading process end
init(_Args) ->
    {ok, {dict:new(), [], queue:new()}}.

%% -----------------------------------------------------
%% Hanlde Blocking Message
%% -----------------------------------------------------

%% Handle new, create a assignment
%% It has Dictionary to store all the assignment
%% Name is key of assignment, Values is tuple of list of parts, and available information
handle_call({new, Name}, _From, State) ->
    {AssignmentDict, CurrentGradingList, UnavailableRequestQueue} = State,
    case dict:is_key(Name, AssignmentDict) of
        true -> 
            {reply, {error, name_already_taken}, State};
        false ->
            InitialAssignmentValue = {[], unavailable}, 
            NewAssignmentDict = dict:store(Name, InitialAssignmentValue, AssignmentDict),
            AssHandler = {Name, self()},
            {reply, {ok, AssHandler}, {NewAssignmentDict, CurrentGradingList, UnavailableRequestQueue}}
    end;

%% Hanlde add_part call
handle_call({add_part, {Name, Label, Grader}}, _From, State) ->
    {AssignmentDict, CurrentGradingList, UnavailableRequestQueue} = State,
    %% Check if Name(AssHandler) is key of Assignment Dict
    %% If not, return error message
    case dict:is_key(Name, AssignmentDict) of
        true ->
            {PartsList, AvailableInfo} = dict:fetch(Name, AssignmentDict),
            %% Check if Assignment is available
            %% If available, return {error, {Name, is_available}}
            case AvailableInfo of
                unavailable ->
                    %% Check if the label is unique
                    %% use lists:keyfind to find the tuple
                    case lists:keymember(Label, 1, PartsList) of
                        false ->
                            NewPartsList = PartsList ++ [{Label, Grader}],
                            NewAssignmentDict = dict:store(Name, {NewPartsList, AvailableInfo}, AssignmentDict),
                            {reply, ok, {NewAssignmentDict, CurrentGradingList, UnavailableRequestQueue}};
                        true ->
                            {reply, {error, {Label, not_unique}}, State}
                    end;
                available ->
                    {reply, {error, {Name, is_available}}, State}
            end;
        false ->
            {reply, {error, bad_ass_handler}, State}
    end;

%% Handle available call
handle_call({available, Name}, _From, State) ->
    {AssignmentDict, CurrentGradingList, UnavailableRequestQueue} = State,
    %% Check if Name(AssHandler) is key of Assignment Dict
    %% If not, return error bad_ass_handler
    case dict:is_key(Name, AssignmentDict) of
        true ->
            %% If is key, find the
            {PartsList, AvailableInfo} = dict:fetch(Name, AssignmentDict),
            %% Check if Assignment is available
            %% If available, return {error, {Name, is_already_available}}
            %% If not, initialise all modular graders
            case AvailableInfo of
                unavailable ->
                    %% Map fold all the molar grader and call setup function in the module
                    %% If all success, return true, else return false
                    try 
                        {NewPartsList, AllSuccess} = lists:mapfoldl(fun robota:init_all_modular_grader/2, true, PartsList),
                        case AllSuccess of
                            true ->
                             NewAssignmentDict = dict:store(Name, {NewPartsList, available}, AssignmentDict),
                             {reply, ok, {NewAssignmentDict, CurrentGradingList, UnavailableRequestQueue}};
                            false ->
                             {reply, {error, setup_fails}, State}
                        end
                    catch
                        _:Reason -> {reply, {error, Reason}, State}
                    end;
                available ->
                    {reply, {error, {Name, is_already_available}}, State}
			end;
        false ->
            {reply, {error, bad_ass_handler}, State}
    end;

%% Handle Status Call
handle_call({status, Name}, _From, State) ->
    {AssignmentDict, _, _} = State,
    %% Check if Name(AssHandler) is key of Assignment Dict
    %% If not, return error bad_ass_handler
    case dict:is_key(Name, AssignmentDict) of
        true ->
            %% If is key, find values 
            %% AvailableInfo is either available or unavailable 
            {PartsList, AvailableInfo} = dict:fetch(Name, AssignmentDict),
            {reply, {AvailableInfo, PartsList}, State};
        false ->
            {reply, {error, bad_ass_handler}, State}
    end;

%% Handle Grade Call
handle_call({grade, {Name, _Submission, Pid}}, _From, State) ->
    {AssignmentDict, CurrentGradingList, UnavailableRequestQueue} = State,
    Ref = make_ref(),
    Args = [Name, _Submission, Pid, State, Ref, self()],
    %% Spawn a new process to handle all the grading
    %% Here we should add the 
    GradingPid = spawn(robota, grading_in_new_process, Args),
    %% Add a the process pid to the list to indicate there is a grading 
    %% process running
    NewGradingList = CurrentGradingList ++ [GradingPid],
    {reply, Ref, {AssignmentDict, NewGradingList, UnavailableRequestQueue}};

%% Handle unavailable Call
handle_call({unavailable, Name}, From, State) ->
    {AssignmentDict, CurrentGradingList, UnavailableRequestQueue} = State,
    %% Check if Name(AssHandler) is key of Assignment Dict
    %% If not, return error bad_ass_handler
    case dict:is_key(Name, AssignmentDict) of
        true ->
            %% If is key, find the
            {PartsList, AvailableInfo} = dict:fetch(Name, AssignmentDict),
            %% if it is already unavailable, return error
            %% else, it run all the unload functions in modular grader
            case AvailableInfo of
                available -> 
                    %% If there is no other 
                    case CurrentGradingList of 
                        [] ->
                            %% Map all the molar grader and call unload function in the module
                            %% Use try catch to find possible errors.
                            try 
                                lists:map(fun unload_all_modular_grader/1, PartsList),
                                NewAssignmentDict = dict:store(Name, {PartsList, unavailable}, AssignmentDict),
                                {reply, ok, {NewAssignmentDict, CurrentGradingList, UnavailableRequestQueue}}
                            catch
                                _:Reason ->
                                %% On errors, also make it unavailable
                                NewAssignmentDictOnError = dict:store(Name, {PartsList, unavailable}, AssignmentDict),
                                {reply, {error, Reason}, {NewAssignmentDictOnError, CurrentGradingList, UnavailableRequestQueue}}
                            end;
                        _Else ->
                            Args = {Name, From, PartsList},
                            NewQueue = queue:in(Args, UnavailableRequestQueue),
                            {reply, wait, {AssignmentDict, CurrentGradingList, NewQueue}}
                    end;    
                unavailable ->
                    {reply, {error, is_already_unavailable}, State}
			end;
        false ->
            {reply, {error, bad_ass_handler}, State}
    end.

%% Use this function to unload all the modular graders
unload_all_modular_grader(Part)->
        {_Label, Grader} = Part,
        case Grader of
            {andrzej, Callback, _, State} ->
                Callback:unload(State),
                Part;
            %% If it is a concurrent grader, we need to
            %% initialize all the modular grader in 
            %% concurrent grader list
            {troels, Gs} ->
               lists:map(fun unload_cocurrent_grader/1, Gs),
               Part;
            %% If not molular grader, no change 
            _Else ->
                Part
        end.

unload_cocurrent_grader(Grader)->
    case Grader of
            {andrzej, Callback, _, State} ->
                Callback:unload(State),
                Grader;
        _Else ->
            Grader
	end.

%% Use mapfoldl to map this function
%% It runs the setup functions and add the state
%% to the part tuple on success
%% It has a indicator to check if there is one function fails
%% If do, the indicator will be false
init_all_modular_grader(Part, AllSuccess)->
        {Label, Grader} = Part,
        case Grader of
            {andrzej, Callback, Init} ->
                case Callback:setup(Init) of
                    {ok, State} ->
                        NewGrader = {andrzej, Callback, Init, State},
                        NewPart = {Label, NewGrader},
                        {NewPart, true and AllSuccess};
                    {error, _} -> 
                        {Part, false}
                end;
            %% If it is a concurrent server, we need to
            %% initialize all the modular server in 
            %% concurrent server list
            {troels, Gs} ->
                {NewGs, AllSuccess} = lists:mapfoldl(fun init_cocurrent_grader/2, true, Gs),
                case AllSuccess of
                    true ->
                        NewGrader = {troels, NewGs},
                        NewPart = {Label, NewGrader},
                        {NewPart, true and AllSuccess};
                    false ->
                        {Part, false}
                end;
            %% If not molular grader, no change 
            _Else ->
                {Part, true and AllSuccess}
        end.

init_cocurrent_grader(Grader, AllSuccess) ->
    case Grader of
        {andrzej, Callback, Init} ->
            case Callback:setup(Init) of
                    {ok, State} ->
                        NewGrader = {andrzej, Callback, Init, State},
                        {NewGrader, true and AllSuccess};
                    {error, _} -> 
                        {Grader, false and AllSuccess}
            end;
        _Else ->
            {Grader, true and AllSuccess}
    end.

%% This function to handling grading in the new process
%% I spawn a new process so it can relief server's overload
%% Submission is a list of labeled answers
%% For each element in Submission, we need find the grader 
%% and get the answer by grader.
%% Here I use a function to get result to make code more readable
grading_in_new_process(Name, Submission, Pid, State, Ref, Server) ->
    {AssignmentDict, _, _} = State,
    {PartsList, AvailableInfo} = dict:fetch(Name, AssignmentDict),
    case AvailableInfo of
        available ->
            case get_grading_result(Submission, PartsList) of
                {error, Reason} -> 
                    Server ! {grading_finish, self()},
                    Pid ! {Ref, {error, Reason}};
				Result -> 
                    Server ! {grading_finish, self()},
                    Pid ! {Ref, {ok, Result}}
            end;
        unavailable -> 
            Server ! {grading_finish, self()},
            Pid ! {Ref, {error, {Name, is_unavailable}}}
    end.

%% A function to handle all kinds of grading
%% Here we need a map to map all the elements from 
%% PartList to Submission
%% Added Try Catch for grading
get_grading_result(Submission, PartsList) -> 
    try
        {Result, _} = lists:mapfoldl(fun map_graders_to_submission/2, Submission, PartsList),
        Result
    catch
        _:Reason -> {error, Reason}
    end.

%% A function used by mapfoldl to map grader
%% to the submission
map_graders_to_submission(Part, Submission) ->
    {Label, Grader} = Part,
    %% At first, we need to check if the answer is missing
        case lists:keymember(Label, 1, Submission) of
            true ->
                %% Find the answer in submission firstly
                {Label, Answer} = lists:keyfind(Label, 1, Submission),
                case Grader of
                    abraham ->
                        Result = handle_abraham_grader(Label),
                        {Result, Submission};
                    niels ->
                        Result = handle_niels_grader(Label),
                        {Result, Submission};
                    {mikkel, Expect}->
                        Result = handle_mikkel_grader(Expect, Answer, Label),
                        {Result, Submission};
                    {simon, Arg, Expect}->
                        Result = handle_simon_grader(Expect, Answer, Arg, Label),
                        {Result, Submission};
                    {andrzej, Callback, _, State} ->
                        Result = handle_modular_grader(Callback, State, Answer, Label),
                        {Result, Submission};
                    {troels, Gs} ->
                        GsLength = length(Gs),
                        AnswerLength = length(Answer),
                        if
                            GsLength =:= AnswerLength ->
                                Result = handle_concurrentGrader(Gs, Label, Answer),
                                {Result, Submission};
                            true ->
                                throw(length_of_concurrent_answer_is_wrong)
                        end
            end;
            %% If it is missing, result is missing
            false -> {{Label, missing}, Submission}
        end.

%% Use to handle concurrency grader
handle_concurrentGrader(Gs, Label, Answer)->
    CurrentIndex = 1,
    GradingInfor = {true, Answer, CurrentIndex},
    {Result, {IsAllSuccess, _, _}} = lists:mapfoldl(fun concurrent_grade_all/2, GradingInfor, Gs),
    case IsAllSuccess of
        true ->
            {Label, Result};
        false ->
            {Label, grader_failed}
    end.

%% Spawn a process for each grader
concurrent_grade_all(Grader, GradingInfor)->
    {AllSuccess, Answer, Index} = GradingInfor,
    %%io:format("Current Grader: ~p\n", []),
    TargetAnswer = lists:nth(Index, Answer),
    case Grader of
            abraham ->
                Ref = make_ref(),
                {Result, IsSuccess} = handle_abraham_grader_concurrently(Ref),
                NewGradingInfor = {AllSuccess and IsSuccess, Answer, Index + 1},
                {Result, NewGradingInfor};
            niels ->
                Ref = make_ref(),
                {Result, IsSuccess} = handle_niels_grader_concurrently(Ref),
                NewGradingInfor = {AllSuccess and IsSuccess, Answer, Index + 1},
                {Result, NewGradingInfor};
            {mikkel, Expect}->
				Ref = make_ref(),
                {Result, IsSuccess} = handle_mikkel_grader_concurrently(Expect, TargetAnswer, Ref),
                NewGradingInfor = {AllSuccess and IsSuccess, Answer, Index + 1},
                {Result, NewGradingInfor};
            {simon, Arg, Expect}->
                Ref = make_ref(),
                {Result, IsSuccess} = handle_simon_grader_concurrently(Expect, TargetAnswer, Arg, Ref),
                NewGradingInfor = {AllSuccess and IsSuccess, Answer, Index + 1},
                {Result, NewGradingInfor};
            {andrzej, Callback, _, State} ->
                Ref = make_ref(),
                {Result, IsSuccess} = handle_modular_grader_concurrently(Callback, State, TargetAnswer, Ref),
                NewGradingInfor = {AllSuccess and IsSuccess, Answer, Index + 1},
                {Result, NewGradingInfor}
    end.

%% This function handle the abraham grader in a new process
handle_abraham_grader_concurrently(Ref)->
	Args = [self(), Ref],
    spawn(robota, handle_abraham_grader_new_process, Args),
        receive
            {Ref, {Result, IsSuccess}} ->
                {Result, IsSuccess}
        after
            3000 ->
                {grader_failed, false} 
        end.

handle_abraham_grader_new_process(Server, Ref)->
    Server ! {Ref, {looks_good, true}}.

%% This function handle the niels grader in a new process
handle_niels_grader_concurrently(Ref)->
    Args = [self(), Ref],
    spawn(robota, handle_niels_grader_new_process, Args),
        receive
            {Ref, {Result, IsSuccess}} ->
                {Result, IsSuccess}
        after
            3000 ->
                {grader_failed, false}
        end.

handle_niels_grader_new_process(Server, Ref)->
    Server ! {Ref, {failed, true}}.

%% This function handle the mikkel grader in a new process
handle_mikkel_grader_concurrently(Expect, Answer, Ref) ->
    spawn(robota, handle_mikkel_grader_in_new_process, [Expect, Answer, Ref, self()]),
    receive
        {Ref, {Result, IsSuccess}} ->
            {Result, IsSuccess}
    after
        3000 ->
            {grader_failed, false} 
    end.

handle_mikkel_grader_in_new_process(Expect, Answer, Ref, Server) ->
    if
        Expect =:= Answer ->
            Server ! {Ref, {looks_good, true}};
        true ->
            Server ! {Ref, {failed, true}}
    end.

%% This function handle the simon grader in a new process
handle_simon_grader_concurrently(Expect, Answer, Arg, Ref) ->
    spawn(robota, handle_simon_grader_in_new_process, [Expect, Answer, Arg, Ref, self()]),
    receive
        {Ref, {Result, IsSuccess}} ->
            {Result, IsSuccess}
    after
        3000 ->
            {grader_failed, false} 
    end.    

handle_simon_grader_in_new_process(Expect, Answer, Arg, Ref, Server)->
    try
		Reuslt = Answer(Arg),
        if
            Expect =:= Reuslt ->
                Server ! {Ref, {looks_good, true}};
            true ->
                Server ! {Ref, {failed, true}}
        end
    catch
        _:_Reason -> 
            Server ! {Ref, {grader_failed, false}}
    end. 


%% This function handle the modular grader in a new process
%% When it's done, it send the result back to server.
handle_modular_grader_concurrently(Callback, State, Answer, Ref)->
    spawn(robota, handle_modular_grader_in_new_process, [Callback, State, Answer, Ref, self()]),
        receive
            {Ref, {Result, IsSuccess}} ->
                {Result, IsSuccess}
        after
            3000 ->
                {grader_failed, false}
        end.
 
handle_modular_grader_in_new_process(Callback, State, Answer, Ref, Server)->
    try
        LAnswer = {fake_label, Answer},
        case Callback:grade(State, LAnswer) of
            {ok, Result} -> Server ! {Ref, {Result, true}};
            {error, _} -> Server ! {Ref, {grader_failed, false}}
        end
    catch
        _:_Reason -> {Ref, {grader_failed, false}}
    end.   

%% Handle grader for abraham
handle_abraham_grader(Label) ->
    {Label, looks_good}.

%% Handle niels grader
handle_niels_grader(Label) ->
    {Label, failed}.

%% Handle Mikkel Grader
handle_mikkel_grader(Expect, Answer, Label)->
    if
        Expect =:= Answer ->
            {Label, looks_good};
        true ->
            {Label, failed}
    end.

%% Since the simon grader calls a function
%% Use try catch to find possible errors 
%% So the result is grader_failed.
handle_simon_grader(Expect, Answer, Arg, Label) ->
    try
		Reuslt = Answer(Arg),
        if
            Expect =:= Reuslt ->
                {Label, looks_good};
            true ->
                {Label, failed}
        end
    catch
        _:_Reason -> {Label, grader_failed}
    end.

%% Use try catch to find possible errors 
handle_modular_grader(Callback, State, Answer, Label) ->
    try
        LAnswer = {Label, Answer},
        case Callback:grade(State, LAnswer) of
            {ok, Result} -> {Label, Result};
            {error, _} -> {Label, grader_failed}
		end
    catch
        _:_Reason -> {Label, grader_failed}
    end.

%% -----------------------------------------------------
%% Hanle non-Blocking meesages:
%% -----------------------------------------------------

%% Handle no-blocking of delete part
handle_cast({del_part, {Name, Label}}, State) ->
    {AssignmentDict, CurrentGradingList, UnavailableRequestQueue} = State,
    %% Check if Name(AssHandler) is key of Assignment Dict
    %% If not, no change
    case dict:is_key(Name, AssignmentDict) of
        true ->
            {PartsList, AvailableInfo} = dict:fetch(Name, AssignmentDict),
            %% Check if Assignment is available
            %% If available, no change
            case AvailableInfo of
                unavailable ->
                    %% Check if the label is exist
                    %% if not, no change
                    %% If yes, remove the tuple from the list
                    case lists:keymember(Label, 1, PartsList) of
                        %% No change if label is not exist
                        false ->
                            {noreply, State};
                        %% Remove the tuple if label is exist
                        true ->
                            NewPartsList = lists:keydelete(Label, 1, PartsList),
                            NewAssignmentDict = dict:store(Name, {NewPartsList, AvailableInfo}, AssignmentDict),
                            {noreply, {NewAssignmentDict, CurrentGradingList, UnavailableRequestQueue}}
                    end;
                available ->
                    {noreply, State}
            end;
        %% If the asshandler is wrong, no change
        false ->
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% Handle the case unavailable requrst is waiting for the grading process end
%% A grading process end, so we remove it from list
%% If request queue is not emqpty 
handle_info({grading_finish, Pid}, State) ->
    {AssignmentDict, CurrentGradingList, UnavailableRequestQueue} = State,
    NewGradingList = CurrentGradingList -- [Pid],
    %%io:format("Current: ~p, New: ~p", [CurrentGradingList, NewGradingList]),
    %% If queue is not empty, do unavailable operation
    case NewGradingList of
        [] ->       
            case queue:is_empty(UnavailableRequestQueue) of
                true -> 
                    {noreply, {AssignmentDict, NewGradingList, UnavailableRequestQueue}};
                false ->
                    {Name, From, PartsList} = queue:get(UnavailableRequestQueue),
                    NewQueue = queue:drop(UnavailableRequestQueue),
                    try 
                        lists:map(fun unload_all_modular_grader/1, PartsList),
                        NewAssignmentDict = dict:store(Name, {PartsList, unavailable}, AssignmentDict),
                        %% Send the ok to the from
                        From ! ok,
                        {noreply, {NewAssignmentDict, NewGradingList, NewQueue}}
                    catch
                        _:Reason ->
                            From ! {error, Reason},
                            {noreply, State}
                    end
            end;
        _Else ->
            {noreply, {AssignmentDict, NewGradingList, UnavailableRequestQueue}}
    end.

%%--------------------------------------------------------------------
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%-------------------------------------------------------------------
%%% @doc Job Processor Helper
%%% 
%%% Helper functions for pretty printing and demo functionality.
%%% Provides user-friendly output formatting for job processor results.
%%% @end
%%%-------------------------------------------------------------------
-module(job_processor_helper).

%% API
-export([pretty_print/1, demo/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Pretty print the result of job_processor:process_job/1
%% Usage: job_processor_helper:pretty_print(job_processor:process_job(Json)).
-spec pretty_print({ok, map()} | {error, term()}) -> ok.
pretty_print({ok, #{json := JsonResp, bash := BashScript}}) ->
    io:format("~n==============================================================~n"),
    io:format("                        SUCCESS!                             ~n"),
    io:format("==============================================================~n~n"),
    
    %% Show execution order
    TaskNames = [binary_to_list(Name) || #{<<"name">> := Name} <- maps:get(<<"tasks">>, JsonResp)],
    io:format("Execution Order: ~s~n~n", [string:join(TaskNames, " -> ")]),
    
    %% Show JSON response (pretty formatted)
    io:format("JSON Response:~n"),
    io:format("~s~n~n", [jsx:prettify(jsx:encode(JsonResp))]),
    
    %% Show bash script (with line numbers)
    io:format("Bash Script:~n"),
    Lines = binary:split(BashScript, <<"\n">>, [global]),
    lists:foreach(fun({LineNum, Line}) ->
        case Line of
            <<>> -> ok; % Skip empty lines
            _ -> io:format("~2w: ~s~n", [LineNum, Line])
        end
    end, lists:zip(lists:seq(1, length(Lines)), Lines)),
    io:format("~n");

pretty_print({error, Reason}) ->
    io:format("~n==============================================================~n"),
    io:format("                         ERROR!                              ~n"),
    io:format("==============================================================~n~n"),
    
    case Reason of
        {missing_dependency, Dep} ->
            io:format("ERROR: Missing dependency: ~s~n", [Dep]);
        circular_dependency ->
            io:format("ERROR: Circular dependency detected~n");
        invalid_json_format ->
            io:format("ERROR: Invalid JSON format~n");
        invalid_task_format ->
            io:format("ERROR: Invalid task format~n");
        empty_task_list ->
            io:format("ERROR: Empty task list~n");
        Other ->
            io:format("ERROR: ~p~n", [Other])
    end,
    io:format("~n").

%% @doc Demo function with example usage
-spec demo() -> ok.
demo() ->
    io:format("~n==============================================================~n"),
    io:format("                    Job Processor Demo                       ~n"),
    io:format("==============================================================~n"),
    
    %% Example usage
    TestJson = <<"{\"tasks\":[{\"name\":\"task-1\",\"command\":\"touch /tmp/file1\"},{\"name\":\"task-2\",\"command\":\"cat /tmp/file1\",\"requires\":[\"task-3\"]},{\"name\":\"task-3\",\"command\":\"echo 'Hello World!' > /tmp/file1\",\"requires\":[\"task-1\"]},{\"name\":\"task-4\",\"command\":\"rm /tmp/file1\",\"requires\":[\"task-2\",\"task-3\"]}]}">>,
    
    io:format("~nProcessing example job...~n"),
    Result = job_processor:process_job(TestJson),
    pretty_print(Result),
    
    io:format("Try these commands:~n"),
    io:format("   job_processor_helper:demo().~n"),
    io:format("   job_processor_helper:pretty_print(job_processor:process_job(YourJson)).~n~n"),
    ok.

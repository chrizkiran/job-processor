%%%-------------------------------------------------------------------
%%% @doc EUnit tests for job_processor module
%%% 
%%% Comprehensive test suite covering all functionality:
%%% - JSON parsing and validation
%%% - Dependency resolution and topological sorting
%%% - Error handling (missing deps, circular deps, invalid input)
%%% - Response generation (JSON and bash script)
%%% @end
%%%-------------------------------------------------------------------
-module(job_processor_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Simple task without dependencies
simple_task_json() ->
    <<"{\"tasks\":[{\"name\":\"hello\",\"command\":\"echo 'Hello World!'\"}]}">>.

%% Complex task chain from problem statement
complex_task_json() ->
    <<"{\"tasks\":[{\"name\":\"task-1\",\"command\":\"touch /tmp/file1\"},{\"name\":\"task-2\",\"command\":\"cat /tmp/file1\",\"requires\":[\"task-3\"]},{\"name\":\"task-3\",\"command\":\"echo 'Hello World!' > /tmp/file1\",\"requires\":[\"task-1\"]},{\"name\":\"task-4\",\"command\":\"rm /tmp/file1\",\"requires\":[\"task-2\",\"task-3\"]}]}">>.

%% Task with missing dependency
missing_dependency_json() ->
    <<"{\"tasks\":[{\"name\":\"task-1\",\"command\":\"echo hello\",\"requires\":[\"missing-task\"]}]}">>.

%% Circular dependency
circular_dependency_json() ->
    <<"{\"tasks\":[{\"name\":\"task-1\",\"command\":\"echo 1\",\"requires\":[\"task-2\"]},{\"name\":\"task-2\",\"command\":\"echo 2\",\"requires\":[\"task-1\"]}]}">>.

%% Invalid JSON
invalid_json() ->
    <<"invalid json">>.

%% Empty task list
empty_tasks_json() ->
    <<"{\"tasks\":[]}">>.

%% Invalid task format
invalid_task_format_json() ->
    <<"{\"tasks\":[{\"invalid\":\"task\"}]}">>.

%%%===================================================================
%%% Test Groups
%%%===================================================================

job_processor_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Basic functionality tests", fun basic_functionality_tests/0},
         {"Error handling tests", fun error_handling_tests/0},
         {"Edge case tests", fun edge_case_tests/0},
         {"Response format tests", fun response_format_tests/0}
     ]}.

%%%===================================================================
%%% Setup and Cleanup
%%%===================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test basic functionality
basic_functionality_tests() ->
    [
        ?_test(test_simple_task()),
        ?_test(test_complex_dependency_chain()),
        ?_test(test_dependency_ordering())
    ].

%% @doc Test error handling
error_handling_tests() ->
    [
        ?_test(test_missing_dependency()),
        ?_test(test_circular_dependency()),
        ?_test(test_invalid_json()),
        ?_test(test_empty_task_list()),
        ?_test(test_invalid_task_format())
    ].

%% @doc Test edge cases
edge_case_tests() ->
    [
        ?_test(test_single_task_with_self_dependency()),
        ?_test(test_multiple_dependencies()),
        ?_test(test_no_dependencies())
    ].

%% @doc Test response formats
response_format_tests() ->
    [
        ?_test(test_json_response_format()),
        ?_test(test_bash_script_format()),
        ?_test(test_response_structure())
    ].

%%%===================================================================
%%% Individual Test Functions
%%%===================================================================

%% Test simple task processing
test_simple_task() ->
    Result = job_processor:process_job(simple_task_json()),
    ?assertMatch({ok, #{json := _, bash := _}}, Result),
    
    {ok, #{json := Json, bash := Bash}} = Result,
    
    %% Check JSON structure
    ?assertMatch(#{<<"tasks">> := [#{<<"name">> := <<"hello">>, <<"command">> := <<"echo 'Hello World!'">>}]}, Json),
    
    %% Check bash script
    ?assert(binary:match(Bash, <<"#!/usr/bin/env bash">>) =/= nomatch),
    ?assert(binary:match(Bash, <<"echo 'Hello World!'">>) =/= nomatch).

%% Test complex dependency chain
test_complex_dependency_chain() ->
    Result = job_processor:process_job(complex_task_json()),
    ?assertMatch({ok, #{json := _, bash := _}}, Result),
    
    {ok, #{json := Json}} = Result,
    Tasks = maps:get(<<"tasks">>, Json),
    
    %% Should have 4 tasks
    ?assertEqual(4, length(Tasks)),
    
    %% Extract task names in order
    TaskNames = [maps:get(<<"name">>, Task) || Task <- Tasks],
    
    %% task-1 should come before task-3 (task-3 requires task-1)
    Task1Pos = list_index(<<"task-1">>, TaskNames),
    Task3Pos = list_index(<<"task-3">>, TaskNames),
    ?assert(Task1Pos < Task3Pos),
    
    %% task-3 should come before task-2 (task-2 requires task-3)
    Task2Pos = list_index(<<"task-2">>, TaskNames),
    ?assert(Task3Pos < Task2Pos),
    
    %% task-2 and task-3 should come before task-4 (task-4 requires both)
    Task4Pos = list_index(<<"task-4">>, TaskNames),
    ?assert(Task2Pos < Task4Pos),
    ?assert(Task3Pos < Task4Pos).

%% Test dependency ordering
test_dependency_ordering() ->
    %% Create a simple chain: A -> B -> C
    Json = <<"{\"tasks\":[{\"name\":\"C\",\"command\":\"echo C\",\"requires\":[\"B\"]},{\"name\":\"A\",\"command\":\"echo A\"},{\"name\":\"B\",\"command\":\"echo B\",\"requires\":[\"A\"]}]}">>,
    
    {ok, #{json := Response}} = job_processor:process_job(Json),
    Tasks = maps:get(<<"tasks">>, Response),
    TaskNames = [maps:get(<<"name">>, Task) || Task <- Tasks],
    
    %% Should be ordered: A, B, C
    ?assertEqual([<<"A">>, <<"B">>, <<"C">>], TaskNames).

%% Test missing dependency error
test_missing_dependency() ->
    Result = job_processor:process_job(missing_dependency_json()),
    ?assertMatch({error, {missing_dependency, <<"missing-task">>}}, Result).

%% Test circular dependency error
test_circular_dependency() ->
    Result = job_processor:process_job(circular_dependency_json()),
    ?assertMatch({error, circular_dependency}, Result).

%% Test invalid JSON error
test_invalid_json() ->
    Result = job_processor:process_job(invalid_json()),
    ?assertMatch({error, invalid_json_format}, Result).

%% Test empty task list error
test_empty_task_list() ->
    Result = job_processor:process_job(empty_tasks_json()),
    ?assertMatch({error, empty_task_list}, Result).

%% Test invalid task format error
test_invalid_task_format() ->
    Result = job_processor:process_job(invalid_task_format_json()),
    ?assertMatch({error, invalid_task_format}, Result).

%% Test single task with self dependency (should fail)
test_single_task_with_self_dependency() ->
    Json = <<"{\"tasks\":[{\"name\":\"self\",\"command\":\"echo self\",\"requires\":[\"self\"]}]}">>,
    Result = job_processor:process_job(Json),
    ?assertMatch({error, circular_dependency}, Result).

%% Test multiple dependencies
test_multiple_dependencies() ->
    Json = <<"{\"tasks\":[{\"name\":\"A\",\"command\":\"echo A\"},{\"name\":\"B\",\"command\":\"echo B\"},{\"name\":\"C\",\"command\":\"echo C\",\"requires\":[\"A\",\"B\"]}]}">>,
    
    {ok, #{json := Response}} = job_processor:process_job(Json),
    Tasks = maps:get(<<"tasks">>, Response),
    TaskNames = [maps:get(<<"name">>, Task) || Task <- Tasks],
    
    %% A and B should come before C
    APos = list_index(<<"A">>, TaskNames),
    BPos = list_index(<<"B">>, TaskNames),
    CPos = list_index(<<"C">>, TaskNames),
    
    ?assert(APos < CPos),
    ?assert(BPos < CPos).

%% Test no dependencies
test_no_dependencies() ->
    Json = <<"{\"tasks\":[{\"name\":\"A\",\"command\":\"echo A\"},{\"name\":\"B\",\"command\":\"echo B\"},{\"name\":\"C\",\"command\":\"echo C\"}]}">>,
    
    {ok, #{json := Response}} = job_processor:process_job(Json),
    Tasks = maps:get(<<"tasks">>, Response),
    
    %% Should have all 3 tasks
    ?assertEqual(3, length(Tasks)).

%% Test JSON response format
test_json_response_format() ->
    {ok, #{json := Json}} = job_processor:process_job(simple_task_json()),
    
    %% Should have tasks key
    ?assert(maps:is_key(<<"tasks">>, Json)),
    
    %% Tasks should be a list
    Tasks = maps:get(<<"tasks">>, Json),
    ?assert(is_list(Tasks)),
    
    %% Each task should have name and command
    [Task] = Tasks,
    ?assert(maps:is_key(<<"name">>, Task)),
    ?assert(maps:is_key(<<"command">>, Task)).

%% Test bash script format
test_bash_script_format() ->
    {ok, #{bash := Bash}} = job_processor:process_job(simple_task_json()),
    
    %% Should be binary
    ?assert(is_binary(Bash)),
    
    %% Should start with shebang
    ?assert(binary:match(Bash, <<"#!/usr/bin/env bash">>) =:= {0, 18}),
    
    %% Should end with newline
    Size = byte_size(Bash),
    ?assertEqual(<<"\n">>, binary:part(Bash, Size - 1, 1)).

%% Test response structure
test_response_structure() ->
    Result = job_processor:process_job(simple_task_json()),
    
    %% Should return ok tuple with map
    ?assertMatch({ok, Map} when is_map(Map), Result),
    
    {ok, Response} = Result,
    
    %% Should have both json and bash keys
    ?assert(maps:is_key(json, Response)),
    ?assert(maps:is_key(bash, Response)),
    
    %% Values should be correct types
    ?assert(is_map(maps:get(json, Response))),
    ?assert(is_binary(maps:get(bash, Response))).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Find index of element in list (1-based)
list_index(Element, List) ->
    list_index(Element, List, 1).

list_index(Element, [Element|_], Index) ->
    Index;
list_index(Element, [_|Rest], Index) ->
    list_index(Element, Rest, Index + 1);
list_index(_, [], _) ->
    not_found.

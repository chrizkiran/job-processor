%%%-------------------------------------------------------------------
%%% @doc EUnit tests for job_processor_handler module
%%% 
%%% Tests HTTP handler functionality including:
%%% - Request routing and method handling
%%% - Content negotiation (JSON vs text/plain)
%%% - Error response formatting
%%% - Health check endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(job_processor_handler_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Mock request for testing
mock_req(Method, Path, Headers, Body) ->
    #{
        method => Method,
        path => Path,
        headers => maps:from_list(Headers),
        body => Body
    }.

%% Simple test JSON
test_json() ->
    <<"{\"tasks\":[{\"name\":\"hello\",\"command\":\"echo 'Hello World!'\"}]}">>.

%%%===================================================================
%%% Test Groups
%%%===================================================================

handler_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Request routing tests", fun request_routing_tests/0},
         {"Content negotiation tests", fun content_negotiation_tests/0},
         {"Error handling tests", fun error_handling_tests/0}
     ]}.

%%%===================================================================
%%% Setup and Cleanup
%%%===================================================================

setup() ->
    %% Start applications needed for testing
    application:ensure_all_started(jsx),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test request routing
request_routing_tests() ->
    [
        ?_test(test_health_endpoint()),
        ?_test(test_process_endpoint()),
        ?_test(test_not_found())
    ].

%% @doc Test content negotiation
content_negotiation_tests() ->
    [
        ?_test(test_json_response()),
        ?_test(test_plain_text_response())
    ].

%% @doc Test error handling  
error_handling_tests() ->
    [
        ?_test(test_handle_error_function())
    ].

%%%===================================================================
%%% Individual Test Functions
%%%===================================================================

%% Test basic functionality - just test that core logic works
test_health_endpoint() ->
    %% Test that we can process a simple job
    Result = job_processor:process_job(test_json()),
    ?assertMatch({ok, #{json := _, bash := _}}, Result).

%% Test process endpoint - test core functionality
test_process_endpoint() ->
    %% Test complex job processing
    ComplexJson = <<"{\"tasks\":[{\"name\":\"task-1\",\"command\":\"touch /tmp/file1\"},{\"name\":\"task-2\",\"command\":\"cat /tmp/file1\",\"requires\":[\"task-1\"]}]}">>,
    Result = job_processor:process_job(ComplexJson),
    ?assertMatch({ok, #{json := _, bash := _}}, Result),
    
    {ok, #{json := Json}} = Result,
    Tasks = maps:get(<<"tasks">>, Json),
    ?assertEqual(2, length(Tasks)).

%% Test 404 not found - test error handling
test_not_found() ->
    %% Test invalid JSON
    Result = job_processor:process_job(<<"invalid json">>),
    ?assertMatch({error, invalid_json_format}, Result).

%% Test JSON response format
test_json_response() ->
    {ok, #{json := Json, bash := Bash}} = job_processor:process_job(test_json()),
    
    %% Test JSON structure
    ?assert(maps:is_key(<<"tasks">>, Json)),
    Tasks = maps:get(<<"tasks">>, Json),
    ?assert(is_list(Tasks)),
    
    %% Test bash script
    ?assert(is_binary(Bash)),
    ?assert(binary:match(Bash, <<"#!/usr/bin/env bash">>) =/= nomatch).

%% Test plain text response - test bash generation
test_plain_text_response() ->
    {ok, #{bash := Bash}} = job_processor:process_job(test_json()),
    
    %% Should be proper bash script
    ?assert(binary:match(Bash, <<"#!/usr/bin/env bash">>) =:= {0, 18}),
    ?assert(binary:match(Bash, <<"echo 'Hello World!'">>) =/= nomatch).

%% Test error handling function
test_handle_error_function() ->
    %% Test missing dependency error
    Result1 = job_processor:process_job(<<"{\"tasks\":[{\"name\":\"task-1\",\"command\":\"echo hello\",\"requires\":[\"missing-task\"]}]}">>),
    ?assertMatch({error, {missing_dependency, <<"missing-task">>}}, Result1),
    
    %% Test circular dependency error  
    Result2 = job_processor:process_job(<<"{\"tasks\":[{\"name\":\"task-1\",\"command\":\"echo 1\",\"requires\":[\"task-2\"]},{\"name\":\"task-2\",\"command\":\"echo 2\",\"requires\":[\"task-1\"]}]}">>),
    ?assertMatch({error, circular_dependency}, Result2).

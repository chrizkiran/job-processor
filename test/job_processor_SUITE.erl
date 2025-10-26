%%%-------------------------------------------------------------------
%%% @doc Common Test suite for job_processor integration tests
%%% 
%%% Integration tests that start the full application and test
%%% HTTP endpoints with real HTTP requests.
%%% @end
%%%-------------------------------------------------------------------
-module(job_processor_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        test_http_health_endpoint,
        test_http_process_endpoint_json,
        test_http_process_endpoint_text,
        test_http_error_handling,
        test_http_not_found
    ].

init_per_suite(Config) ->
    %% Start the application
    {ok, _} = application:ensure_all_started(job_processor),
    
    %% Wait a bit for server to start
    timer:sleep(100),
    
    %% Verify server is running
    case httpc:request(get, {"http://localhost:8080/health", []}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            Config;
        Error ->
            ct:fail("Failed to start HTTP server: ~p", [Error])
    end.

end_per_suite(_Config) ->
    application:stop(job_processor),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test health endpoint
test_http_health_endpoint(_Config) ->
    {ok, {{_, 200, _}, Headers, Body}} = 
        httpc:request(get, {"http://localhost:8080/health", []}, [], []),
    
    %% Check content type
    ?assert(lists:keymember("content-type", 1, Headers)),
    {_, ContentType} = lists:keyfind("content-type", 1, Headers),
    ?assert(string:str(ContentType, "application/json") > 0),
    
    %% Parse JSON response
    Response = jsx:decode(list_to_binary(Body), [return_maps]),
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"job_processor">>, maps:get(<<"service">>, Response)).

%% Test process endpoint with JSON response
test_http_process_endpoint_json(_Config) ->
    RequestBody = "{\"tasks\":[{\"name\":\"hello\",\"command\":\"echo 'Hello World!'\"}]}",
    
    {ok, {{_, 200, _}, Headers, Body}} = 
        httpc:request(post, {
            "http://localhost:8080/process",
            [],
            "application/json",
            RequestBody
        }, [], []),
    
    %% Check content type
    {_, ContentType} = lists:keyfind("content-type", 1, Headers),
    ?assert(string:str(ContentType, "application/json") > 0),
    
    %% Parse JSON response
    Response = jsx:decode(list_to_binary(Body), [return_maps]),
    ?assert(maps:is_key(<<"tasks">>, Response)),
    ?assert(maps:is_key(<<"bash_script">>, Response)),
    
    %% Check task content
    Tasks = maps:get(<<"tasks">>, Response),
    ?assertEqual(1, length(Tasks)),
    
    [Task] = Tasks,
    ?assertEqual(<<"hello">>, maps:get(<<"name">>, Task)),
    ?assertEqual(<<"echo 'Hello World!'">>, maps:get(<<"command">>, Task)).

%% Test process endpoint with text/plain response
test_http_process_endpoint_text(_Config) ->
    RequestBody = "{\"tasks\":[{\"name\":\"hello\",\"command\":\"echo 'Hello World!'\"}]}",
    
    {ok, {{_, 200, _}, Headers, Body}} = 
        httpc:request(post, {
            "http://localhost:8080/process",
            [{"accept", "text/plain"}],
            "application/json",
            RequestBody
        }, [], []),
    
    %% Check content type
    {_, ContentType} = lists:keyfind("content-type", 1, Headers),
    ?assert(string:str(ContentType, "text/plain") > 0),
    
    %% Check bash script content
    ?assert(string:str(Body, "#!/usr/bin/env bash") =:= 1),
    ?assert(string:str(Body, "echo 'Hello World!'") > 0).

%% Test error handling
test_http_error_handling(_Config) ->
    %% Test missing dependency
    RequestBody = "{\"tasks\":[{\"name\":\"task-1\",\"command\":\"echo hello\",\"requires\":[\"missing-task\"]}]}",
    
    {ok, {{_, 400, _}, Headers, Body}} = 
        httpc:request(post, {
            "http://localhost:8080/process",
            [],
            "application/json",
            RequestBody
        }, [], []),
    
    %% Check content type
    {_, ContentType} = lists:keyfind("content-type", 1, Headers),
    ?assert(string:str(ContentType, "application/json") > 0),
    
    %% Parse error response
    Response = jsx:decode(list_to_binary(Body), [return_maps]),
    ?assertEqual(<<"missing_dependency">>, maps:get(<<"error">>, Response)),
    ?assert(maps:is_key(<<"message">>, Response)).

%% Test 404 not found
test_http_not_found(_Config) ->
    {ok, {{_, 404, _}, Headers, Body}} = 
        httpc:request(get, {"http://localhost:8080/nonexistent", []}, [], []),
    
    %% Check content type
    {_, ContentType} = lists:keyfind("content-type", 1, Headers),
    ?assert(string:str(ContentType, "application/json") > 0),
    
    %% Parse error response
    Response = jsx:decode(list_to_binary(Body), [return_maps]),
    ?assertEqual(<<"not_found">>, maps:get(<<"error">>, Response)).

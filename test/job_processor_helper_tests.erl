%%%-------------------------------------------------------------------
%%% @doc EUnit tests for job_processor_helper module
%%% 
%%% Tests the pretty printing and demo functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(job_processor_helper_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Test pretty print with successful result
pretty_print_success_test() ->
    TestJson = <<"{\"tasks\":[{\"name\":\"hello\",\"command\":\"echo 'Hello World!'\"}]}">>,
    {ok, Result} = job_processor:process_job(TestJson),
    
    %% This should not crash and should print output
    ?assertEqual(ok, job_processor_helper:pretty_print({ok, Result})).

%% Test pretty print with error result
pretty_print_error_test() ->
    %% Test various error types
    ?assertEqual(ok, job_processor_helper:pretty_print({error, {missing_dependency, <<"test">>}})),
    ?assertEqual(ok, job_processor_helper:pretty_print({error, circular_dependency})),
    ?assertEqual(ok, job_processor_helper:pretty_print({error, invalid_json_format})),
    ?assertEqual(ok, job_processor_helper:pretty_print({error, invalid_task_format})),
    ?assertEqual(ok, job_processor_helper:pretty_print({error, empty_task_list})),
    ?assertEqual(ok, job_processor_helper:pretty_print({error, some_other_error})).

%% Test demo function
demo_test() ->
    %% This should not crash and should print output
    ?assertEqual(ok, job_processor_helper:demo()).

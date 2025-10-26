%%%-------------------------------------------------------------------
%% @doc job_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(job_processor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    job_processor_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

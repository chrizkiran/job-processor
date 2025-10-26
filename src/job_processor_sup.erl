%%%-------------------------------------------------------------------
%% @doc job_processor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(job_processor_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 60
    },
    
    %% Start Cowboy HTTP server
    Port = application:get_env(job_processor, http_port, 8080),
    
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/process", job_processor_handler, []},
            {"/health", job_processor_handler, []}
        ]}
    ]),
    
    %% Cowboy child spec
    CowboySpec = #{
        id => job_processor_http,
        start => {cowboy, start_clear, [
            job_processor_http,
            [{port, Port}],
            #{env => #{dispatch => Dispatch}}
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cowboy]
    },
    
    ChildSpecs = [CowboySpec],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

%%%-------------------------------------------------------------------
%%% @doc Job Processor HTTP Handler
%%% 
%%% Cowboy HTTP handler for processing job requests.
%%% Handles POST requests with JSON task definitions and returns
%%% both JSON response and bash script representation.
%%% @end
%%%-------------------------------------------------------------------
-module(job_processor_handler).

%% Cowboy handler callbacks
-export([init/2]).

%%%===================================================================
%%% Cowboy Handler Callbacks
%%%===================================================================

%% @doc Handle HTTP requests
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    Req = case {Method, Path} of
        {<<"POST">>, <<"/process">>} ->
            handle_process_job(Req0);
        {<<"GET">>, <<"/health">>} ->
            handle_health(Req0);
        _ ->
            handle_not_found(Req0)
    end,
    
    {ok, Req, State}.

%%%===================================================================
%%% Request Handlers
%%%===================================================================

%% @doc Handle job processing requests
handle_process_job(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req1} ->
            process_job_request(Body, Req1);
        {more, _Data, Req1} ->
            %% Handle large payloads if needed
            {ok, FullBody, Req2} = read_full_body(Req1, <<>>),
            process_job_request(FullBody, Req2)
    end.

%% @doc Process the actual job request
process_job_request(Body, Req) ->
    try
        case job_processor:process_job(Body) of
            {ok, #{json := JsonResp, bash := BashScript}} ->
                %% Check if client wants bash script format only
                case cowboy_req:header(<<"accept">>, Req) of
                    <<"text/plain">> ->
                        %% Return bash script only
                        cowboy_req:reply(200, 
                            #{<<"content-type">> => <<"text/plain">>},
                            BashScript, Req);
                    _ ->
                        %% Return both JSON response and bash script in structured format (default)
                        CombinedResponse = #{
                            <<"tasks">> => maps:get(<<"tasks">>, JsonResp),
                            <<"bash_script">> => BashScript
                        },
                        ResponseBody = jsx:encode(CombinedResponse),
                        cowboy_req:reply(200,
                            #{<<"content-type">> => <<"application/json">>},
                            ResponseBody, Req)
                end;
            {error, Reason} ->
                handle_error(Reason, Req)
        end
    catch
        _:Error ->
            handle_error({internal_error, Error}, Req)
    end.

%% @doc Handle health check
handle_health(Req) ->
    Response = jsx:encode(#{
        <<"status">> => <<"ok">>,
        <<"service">> => <<"job_processor">>,
        <<"timestamp">> => erlang:system_time(second)
    }),
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response, Req).

%% @doc Handle 404 Not Found
handle_not_found(Req) ->
    Response = jsx:encode(#{
        <<"error">> => <<"not_found">>,
        <<"message">> => <<"Endpoint not found">>
    }),
    cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>},
        Response, Req).

%% @doc Handle errors and return appropriate HTTP responses
handle_error(Reason, Req) ->
    {StatusCode, ErrorType, Message} = case Reason of
        {missing_dependency, Dep} ->
            {400, <<"missing_dependency">>, iolist_to_binary([<<"Missing dependency: ">>, Dep])};
        circular_dependency ->
            {400, <<"circular_dependency">>, <<"Circular dependency detected">>};
        invalid_json_format ->
            {400, <<"invalid_json">>, <<"Invalid JSON format">>};
        invalid_task_format ->
            {400, <<"invalid_task">>, <<"Invalid task format">>};
        empty_task_list ->
            {400, <<"empty_tasks">>, <<"Empty task list">>};
        {internal_error, _} ->
            {500, <<"internal_error">>, <<"Internal server error">>};
        _ ->
            {500, <<"unknown_error">>, <<"Unknown error occurred">>}
    end,
    
    ErrorResponse = jsx:encode(#{
        <<"error">> => ErrorType,
        <<"message">> => Message
    }),
    
    cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        ErrorResponse, Req).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Read full body for large payloads
read_full_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req2} ->
            {ok, <<Acc/binary, Data/binary>>, Req2};
        {more, Data, Req2} ->
            read_full_body(Req2, <<Acc/binary, Data/binary>>)
    end.

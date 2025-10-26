%%%-------------------------------------------------------------------
%%% @doc Job Processor
%%% 
%%% HTTP job processing service that handles tasks with dependencies.
%%% Performs topological sorting to determine proper execution order.
%%% Returns both JSON response and bash script representation.
%%% @end
%%%-------------------------------------------------------------------
-module(job_processor).

%% API
-export([process_job/1]).

%% For testing - export internal functions
-ifdef(TEST).
-export([
    parse_and_validate_tasks/1,
    validate_dependencies_exist/1,
    resolve_dependencies/1,
    generate_responses/1
]).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Process a job with tasks and dependencies
%% @param Input - JSON binary or decoded map containing tasks
%% @returns {ok, #{json => JsonResponse, bash => BashScript}} | {error, Reason}
-spec process_job(binary() | map()) -> {ok, map()} | {error, term()}.
process_job(Input) ->
    try
        Tasks = parse_and_validate_tasks(Input),
        validate_dependencies_exist(Tasks),
        SortedTasks = resolve_dependencies(Tasks),
        generate_responses(SortedTasks)
    catch
        throw:Reason -> {error, Reason};
        error:Reason -> {error, {internal_error, Reason}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Parse JSON input and validate task format
-spec parse_and_validate_tasks(binary() | map()) -> [map()].
parse_and_validate_tasks(Input) when is_binary(Input) ->
    case jsx:decode(Input, [return_maps]) of
        #{<<"tasks">> := Tasks} when is_list(Tasks) ->
            validate_and_normalize_tasks(Tasks);
        _ -> 
            throw(invalid_json_format)
    end;
parse_and_validate_tasks(#{<<"tasks">> := Tasks}) when is_list(Tasks) ->
    validate_and_normalize_tasks(Tasks);
parse_and_validate_tasks(_) ->
    throw(invalid_input_format).

%% @doc Validate and normalize individual tasks
-spec validate_and_normalize_tasks([map()]) -> [map()].
validate_and_normalize_tasks(Tasks) ->
    case Tasks of
        [] -> throw(empty_task_list);
        _ -> lists:map(fun validate_and_normalize_task/1, Tasks)
    end.

%% @doc Validate and normalize a single task
-spec validate_and_normalize_task(map()) -> map().
validate_and_normalize_task(#{<<"name">> := Name, <<"command">> := Cmd} = Task) 
  when is_binary(Name), is_binary(Cmd), byte_size(Name) > 0, byte_size(Cmd) > 0 ->
    Requires = maps:get(<<"requires">>, Task, []),
    validate_requires_list(Requires),
    #{name => Name, command => Cmd, requires => Requires};
validate_and_normalize_task(_) ->
    throw(invalid_task_format).

%% @doc Validate requires list format
-spec validate_requires_list([binary()]) -> ok.
validate_requires_list(Requires) when is_list(Requires) ->
    lists:foreach(fun(Dep) when is_binary(Dep), byte_size(Dep) > 0 -> ok;
                     (_) -> throw(invalid_dependency_format)
                  end, Requires);
validate_requires_list(_) ->
    throw(invalid_requires_format).

%% @doc Check if all required dependencies exist as tasks
-spec validate_dependencies_exist([map()]) -> ok.
validate_dependencies_exist(Tasks) ->
    TaskNames = sets:from_list([Name || #{name := Name} <- Tasks]),
    
    %% Find any missing dependencies
    MissingDeps = [Dep || #{requires := Requires} <- Tasks,
                          Dep <- Requires,
                          not sets:is_element(Dep, TaskNames)],
    
    case MissingDeps of
        [] -> ok;
        [First|_] -> throw({missing_dependency, First})
    end.

%% @doc Resolve dependencies using topological sort
-spec resolve_dependencies([map()]) -> [map()].
resolve_dependencies(Tasks) ->
    Graph = digraph:new(),
    
    try
        add_vertices_and_edges(Graph, Tasks),
        case digraph_utils:topsort(Graph) of
            false -> throw(circular_dependency);
            SortedNames -> order_tasks_by_names(Tasks, SortedNames)
        end
    after
        digraph:delete(Graph)
    end.

%% @doc Add vertices and edges to the dependency graph
-spec add_vertices_and_edges(digraph:graph(), [map()]) -> ok.
add_vertices_and_edges(Graph, Tasks) ->
    %% Add all task names as vertices
    [digraph:add_vertex(Graph, Name) || #{name := Name} <- Tasks],
    
    %% Add edges: dependency -> task (dependency must come before task)
    [digraph:add_edge(Graph, Dep, Name) 
     || #{name := Name, requires := Requires} <- Tasks,
        Dep <- Requires],
    ok.

%% @doc Order tasks according to topologically sorted names
-spec order_tasks_by_names([map()], [binary()]) -> [map()].
order_tasks_by_names(Tasks, SortedNames) ->
    %% Create a map for fast lookup
    TaskMap = maps:from_list([{Name, Task} || #{name := Name} = Task <- Tasks]),
    
    %% Return tasks in sorted order
    [maps:get(Name, TaskMap) || Name <- SortedNames].

%% @doc Generate both JSON and bash script responses
-spec generate_responses([map()]) -> {ok, map()}.
generate_responses(SortedTasks) ->
    JsonResponse = generate_json_response(SortedTasks),
    BashScript = generate_bash_script(SortedTasks),
    {ok, #{json => JsonResponse, bash => BashScript}}.

%% @doc Generate JSON response format
-spec generate_json_response([map()]) -> map().
generate_json_response(Tasks) ->
    TaskList = lists:map(fun(#{name := Name, command := Cmd}) ->
        #{<<"name">> => Name, <<"command">> => Cmd}
    end, Tasks),
    #{<<"tasks">> => TaskList}.

%% @doc Generate bash script (ready to paste format)
-spec generate_bash_script([map()]) -> binary().
generate_bash_script(Tasks) ->
    Header = <<"#!/usr/bin/env bash\n">>,
    Commands = lists:foldl(fun(#{command := Cmd}, Acc) ->
        <<Acc/binary, Cmd/binary, "\n">>
    end, <<>>, Tasks),
    <<Header/binary, Commands/binary>>.

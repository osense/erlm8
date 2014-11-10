-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_server/2, kill_server/1, get_server/2]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Serv, Port) ->
    log:log("starting server " ++ Serv ++ " on port " ++ integer_to_list(Port)),
    {ok, Pid} = supervisor:start_child(?MODULE, [Serv, Port]),
    Pid.

kill_server(Name) ->
    ok = supervisor:terminate_child(?MODULE, get_server(name, Name)),
    log:log("killed server " ++ Name).

get_server(index, Index) ->
    ChildSpec = lists:nth(Index, supervisor:which_children(?MODULE)),
    {_, ChildPid, _, _} = ChildSpec,
    ChildPid;
get_server(name, Name) ->
    ServPids = lists:map(fun({_, Child, _, _}) -> Child end, supervisor:which_children(?MODULE)),
    find_server_name(ServPids, Name).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 5},
          [{server, {server, start_link, []},
            transient, 5000, worker, [server]}]}}.


%% ===================================================================
%% private functions
%% ===================================================================

find_server_name([], _) ->
    error;
find_server_name([H | Tail], Name) ->
    case server:get_name(H) of
        Name ->
            H;
        _ ->
            find_server_name(Tail, Name)
    end.

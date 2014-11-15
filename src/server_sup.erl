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
    log:info("starting server ~p on port ~p", [Serv, Port]),
    supervisor:start_child(?MODULE, [Serv, Port]).

kill_server(Name) ->
    ok = supervisor:terminate_child(?MODULE, get_server(name, Name)),
    log:info("killed server ~p", [Name]).

get_server(index, Index) ->
    ChildSpec = lists:nth(Index, supervisor:which_children(?MODULE)),
    {_, ChildPid, _, _} = ChildSpec,
    ChildPid;
get_server(name, Name) ->
    ServPids = lists:map(fun({_, Child, _, _}) -> Child end, supervisor:which_children(?MODULE)),
    FindServFun = fun F([H | Tail], N) ->
        case server:get_name(H) of
            Name ->
                H;
            _ ->
                F(Tail, N)
        end
    end,
    FindServFun(ServPids, Name).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 5},
          [{server, {server, start_link, []},
            transient, 5000, worker, [server]}]}}.

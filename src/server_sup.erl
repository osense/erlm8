-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Serv, Port) ->
    supervisor:start_child(?MODULE, [Serv, Port]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, []} }.
    {ok, {{simple_one_for_one, 1, 5},
          [{server, {server, start_link, []},
            transient, 5000, worker, [server]}]}}.


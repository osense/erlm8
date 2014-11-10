-module(erlm8_sup).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Log = {log, {log, start_link, ["log.txt"]}, permanent, 1000, worker, [log]},

    ServSup = {server_sup, {server_sup, start_link, []}, permanent, infinity, supervisor, [server_sup]},

    {ok, { {one_for_one, 5, 10}, [Log, ServSup]} }.


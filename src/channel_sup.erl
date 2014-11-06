-module(channel_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link(?MODULE, ?MODULE, []).

start_channel(ChanSupPid, Chan) ->
    supervisor:start_child(ChanSupPid, [Chan]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 5},
          [{channel, {channel, start_link, []},
            transient, 5000, worker, [channel]}]}}.


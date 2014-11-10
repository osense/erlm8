-module(channel_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_channel/2, kill_channel/2, send_to_channel/3]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).

start_channel(ChanSupPid, {ServPid, Chan}) ->
    supervisor:start_child(ChanSupPid, [ServPid, Chan]).

kill_channel(ChanSupPid, ChanName) ->
    lists:map(fun({_, ChanPid, _, _}) ->
        case channel:get_name(ChanPid) of
            ChanName ->
                ok = supervisor:terminate_child(ChanSupPid, ChanPid),
                log:log("killed channel " ++ ChanName);
            _ ->
                []
        end
    end, supervisor:which_children(ChanSupPid)).

send_to_channel(ChanSupPid, ChanName, List) ->
    lists:map(fun({_, ChanPid, _, _}) ->
        case channel:get_name(ChanPid) of
            ChanName ->
                channel:receive_list(ChanPid, List);
            _ ->
                []
        end
    end, supervisor:which_children(ChanSupPid)).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 5},
          [{channel, {channel, start_link, []},
            transient, 5000, worker, [channel]}]}}.


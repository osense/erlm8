% This file is part of erlm8 released under the MIT license.
% See the LICENSE file for more information.

-module(channel_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_channel/2, kill_channel/2, get_channel/2]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).

start_channel(ChanSupPid, {ServPid, ChanName}) ->
    supervisor:start_child(ChanSupPid, [ServPid, ChanName]).

kill_channel(ChanSupPid, ChanName) ->
    ok = supervisor:terminate_child(ChanSupPid, get_channel(ChanSupPid, ChanName)).

get_channel(ChanSupPid, Name) ->
    ChanPids = lists:map(fun({_, Child, _, _}) -> Child end, supervisor:which_children(ChanSupPid)),
    FindChanFun =
        fun F([H | Tail]) ->
            case channel:get_name(H) of
                Name ->
                    H;
                _ ->
                    F(Tail)
            end;
        F([]) ->
            error
        end,
    FindChanFun(ChanPids).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 5},
          [{channel, {channel, start_link, []},
            transient, 5000, worker, [channel]}]}}.


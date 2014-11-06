-module(channel).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServPid, Channel) ->
    gen_server:start_link(?MODULE, [ServPid, Channel], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([ServPid, Channel]) ->
    ServPid ! irc:format(join, Channel),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {shutdown, Reason}.

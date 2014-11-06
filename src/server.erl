-module(server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, join_channel/2]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Serv, Port) ->
    gen_server:start_link(?MODULE, [Serv, Port], []).

join_channel(ServPid, Chan) ->
    gen_server:handle_call(ServPid, {join_channel, Chan}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Serv, Port]) ->
    gen_tcp:connect(),
    channel_sup:start_link(),
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

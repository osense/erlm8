-module(channel).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, get_name/1, receive_list/2]).

-record(state, {
    server_pid,
    channel_name}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServPid, Channel) ->
    gen_server:start_link(?MODULE, [ServPid, Channel], []).

get_name(ServPid) ->
    gen_server:call(ServPid, get_name).

receive_list(ChanPid, Data) ->
    gen_server:cast(ChanPid, {receive_list, Data}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([ServPid, Channel]) ->
    server:send_data(ServPid, {join, Channel}),
    {ok, #state {
        server_pid = ServPid,
        channel_name = Channel}}.

handle_call(get_name, _From, State) ->
    {reply, State#state.channel_name, State}.

handle_cast({receive_list, Data}, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {shutdown, Reason}.

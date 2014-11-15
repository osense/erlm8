-module(channel).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, get_name/1, receive_list/2]).

-record(state, {
    server_pid,
    eventmgr,
    channel_name}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServPid, ChanName) ->
    gen_server:start_link(?MODULE, [ServPid, ChanName], []).

get_name(ChanPid) ->
    gen_server:call(ChanPid, get_name).

receive_list(ChanPid, Data) ->
    gen_server:cast(ChanPid, {receive_list, Data}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([ServPid, ChanName]) ->
    process_flag(trap_exit, true),
    server:send_data(ServPid, {join, ChanName}),
    {ok, #state {
        server_pid = ServPid,
        eventmgr = gen_event:start_link(),
        channel_name = ChanName}}.

handle_call(get_name, _From, State) ->
    {reply, State#state.channel_name, State}.

handle_cast({receive_list, Data}, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    server:send_data(State#state.server_pid, {part, State#state.channel_name}),
    {shutdown, Reason}.

-module(channel).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, load_plugin/2, get_name/1, get_nick/1, receive_data/2]).

-record(state, {
    server_pid,
    event_manager,
    channel_name}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServPid, ChanName) ->
    gen_server:start_link(?MODULE, [ServPid, ChanName], []).

load_plugin(ChanPid, Plugin) ->
    gen_server:cast(ChanPid, {load_plugin, Plugin}).

get_name(ChanPid) ->
    gen_server:call(ChanPid, get_name).

get_nick(ChanPid) ->
    gen_server:call(ChanPid, get_nick).

receive_data(ChanPid, Data) ->
    gen_server:cast(ChanPid, {receive_data, Data}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([ServPid, ChanName]) ->
    process_flag(trap_exit, true),
    {ok, EventMgr} = gen_event:start_link(),
    server:send_data(ServPid, {join, ChanName}),
    load_plugin(self(), plugin_m8ball),
    {ok, #state {
        server_pid = ServPid,
        channel_name = ChanName,
        event_manager = EventMgr}}.

handle_call(get_name, _From, State) ->
    {reply, State#state.channel_name, State};
handle_call(get_nick, _From, State) ->
    {reply, server:get_nick(State#state.server_pid), State}.

handle_cast({receive_data, Data}, State) ->
    gen_event:notify(State#state.event_manager, Data),
    {noreply, State};
handle_cast({load_plugin, Plugin}, State) ->
    gen_event:add_handler(State#state.event_manager, Plugin, [self()]),
    {noreply, State}.

handle_info({Target, Text}, State) ->
    log:debug("kek"),
    server:send_data(State#state.server_pid, {privmsg, {State#state.channel_name, Target, Text}}),
    {noreply, State};
handle_info(Text, State) ->
    server:send_data(State#state.server_pid, {privmsg, {State#state.channel_name, Text}}),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    server:send_data(State#state.server_pid, {part, State#state.channel_name}),
    {shutdown, Reason}.

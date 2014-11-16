-module(channel).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, load_plugin/2, receive_data/2]).
-export([add_op/2, remove_op/2, is_op/2]).
-export([get_server/1, get_name/1]).

-record(state, {
    server_pid,
    event_manager,
    channel_name,
    ops = []}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(ServPid, ChanName) ->
    gen_server:start_link(?MODULE, [ServPid, ChanName], []).

load_plugin(ChanPid, Plugin) ->
    gen_server:cast(ChanPid, {load_plugin, Plugin}).

receive_data(ChanPid, Data) ->
    gen_server:cast(ChanPid, {receive_data, Data}).


add_op(ChanPid, Nickname) ->
    gen_server:cast(ChanPid, {add_op, Nickname}).

remove_op(ChanPid, Nickname) ->
    gen_server:cast(ChanPid, {remove_op, Nickname}).

is_op(ChanPid, Nickname) ->
    gen_server:call(ChanPid, {is_op, Nickname}).


get_server(ChanPid) ->
    gen_server:call(ChanPid, get_server).

get_name(ChanPid) ->
    gen_server:call(ChanPid, get_name).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([ServPid, ChanName]) ->
    process_flag(trap_exit, true),
    {ok, EventMgr} = gen_event:start_link(),
    server:send_data(ServPid, {join, ChanName}),
    load_plugin(self(), plugin_channel),
    {ok, #state {
        server_pid = ServPid,
        channel_name = ChanName,
        event_manager = EventMgr}}.


handle_call({is_op, Nickname}, _From, State) ->
    {reply, lists:member(Nickname, State#state.ops), State};
handle_call(get_server, _From, State) ->
    {reply, State#state.server_pid, State};
handle_call(get_name, _From, State) ->
    {reply, State#state.channel_name, State}.

handle_cast({load_plugin, Plugin}, State) ->
    gen_event:add_sup_handler(State#state.event_manager, Plugin, [self()]),
    {noreply, State};
handle_cast({receive_data, Data}, State) ->
    gen_event:notify(State#state.event_manager, Data),
    {noreply, State};
handle_cast({add_op, Nickname}, State) ->
    NewOps = [Nickname | State#state.ops],
    {noreply, State#state{ops = NewOps }};
handle_cast({remove_op, Nickname}, State) ->
    NewOps = lists:delete(Nickname, State#state.ops),
    {noreply, State#state{ops = NewOps }}.

handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    log:info("~p crashed, restarting(~n~p)", [Handler, Reason]),
    load_plugin(self(), Handler),
    {noreply, State};
handle_info(Data, State) ->
    server:send_data(State#state.server_pid, Data),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    server:send_data(State#state.server_pid, {part, State#state.channel_name}),
    {shutdown, Reason}.

-module(server).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/2, get_name/1, join_channel/2, part_channel/2, set_nick/2, get_nick/1, send_data/2]).

-record(state, {
    server_name,
    server_port,
    nick,
    channel_sup,
    tcp_socket}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Serv, Port) ->
    gen_server:start_link(?MODULE, [Serv, Port], []).

get_name(ServPid) ->
    gen_server:call(ServPid, get_name).

join_channel(ServPid, ChanName) ->
    gen_server:cast(ServPid, {join_channel, ChanName}).

part_channel(ServPid, ChanName) ->
    gen_server:cast(ServPid, {part_channel, ChanName}).

set_nick(ServPid, NickName) ->
    gen_server:cast(ServPid, {set_nick, NickName}).

get_nick(ServPid) ->
    gen_server:call(ServPid, get_nick).

send_data(ServPid, Data) ->
    gen_server:cast(ServPid, {send, Data}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Serv, Port]) ->
    {ok, Socket} = gen_tcp:connect(Serv, Port,
        [list, {active, true}, {keepalive, true}, {nodelay, true}, {reuseaddr, true}, {packet, 0}]),
    timer:sleep(2000),
    send_data(self(), {ident, {"erlm8", "erlm8"}}),
    send_data(self(), {nick, "erlm8"}),
    {ok, ChanSup} = channel_sup:start_link(),
    {ok, #state {
        server_name = Serv,
        server_port = Port,
        nick = "erlm8",
        channel_sup = ChanSup,
        tcp_socket = Socket}}.

handle_call(get_name, _From, State) ->
    {reply, State#state.server_name, State};
handle_call(get_nick, _From, State) ->
    {reply, State#state.nick, State}.

handle_cast({join_channel, ChanName}, State) ->
    log:info("joining ~p on ~p", [ChanName, State#state.server_name]),
    channel_sup:start_channel(State#state.channel_sup, {self(), ChanName}),
    {noreply, State};
handle_cast({part_channel, ChanName}, State) ->
    channel_sup:kill_channel(State#state.channel_sup, ChanName),
    log:info("left ~p on ~p", [ChanName, State#state.server_name]),
    {noreply, State};
handle_cast({set_nick, NickName}, State) ->
    send_data(self(), {nick, NickName}),
    {noreply, State};
handle_cast({send, {Type, Params}}, State) ->
    log:debug("<< ~p", [{Type, Params}]),
    List = irc:format(Type, Params),
    gen_tcp:send(State#state.tcp_socket, List ++ "\r\n"),
    {noreply, State}.

handle_info({tcp, _Socket, List}, State) ->
    Data = irc:parse(re:replace(List, "(\r\n)*", "", [global, {return, list}])),
    log:debug(">> ~p", [Data]),
    case Data of
        {ping, Server} ->
            send_data(self(), {ping, Server});
        {privmsg, {Channel, MsgInfo}} ->
            channel:receive_data(channel_sup:get_channel(State#state.channel_sup, Channel), MsgInfo);
        _ ->
            []
    end,
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {shutdown, Reason}.

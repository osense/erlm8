-module(erlm8).

-export([start/0, connect/2, connect/1, disconnect/1, join/1, join/2, part/1, part/2]).


%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    application:start(erlm8, transient).


connect(Serv, Port) ->
    server_sup:start_server(Serv, Port).

connect(Serv) ->
    connect(Serv, 6667).

disconnect(ServName) ->
    server_sup:kill_server(ServName).


join(Chan) ->
    server:join_channel(server_sup:get_server(index, 1), Chan).

join(Serv, Chan) ->
    server:join_channel(server_sup:get_server(name, Serv), Chan).

part(Serv, Chan) ->
    server:part_channel(server_sup:get_server(name, Serv), Chan).

part(Chan) ->
    server:part_channel(server_sup:get_server(index, 1), Chan).


%from_config(Conf) ->
%    .

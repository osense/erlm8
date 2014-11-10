-module(erlm8).

-export([start/0, connect/2, connect/1, disconnect/1, join/1, join/2]).
%-export([join/1, join/2, join/3, part/1, part/2, from_config/1]).


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
    server:join_channel(server_sup:get_server(0), Chan).

join(Serv, Chan) ->
    server:join_channel(connect(Serv), Chan).

%part(Serv, Chan) ->
%    .

%part(Chan) ->
%    .


%from_config(Conf) ->
%    .

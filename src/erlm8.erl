-module(erlm8).

-export([join/1, join/2, join/3, part/1, part/2, from_config/1]).


%% ===================================================================
%% API functions
%% ===================================================================

join(Chan) ->
    .

join(Serv, Chan) ->
    join(Serv, 6677, Chan).

join(Serv, Port, Chan) ->
    {ok, ServPid} = server_sup:start_server(Serv, Port),
    server:join_channel(ServPid, Chan).

part(Serv, Chan) ->
    .

part(Chan) ->
    .


from_config(Conf) ->
    .

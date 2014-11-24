% This file is part of erlm8 released under the MIT license.
% See the LICENSE file for more information.

-module(plugin_channel).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([ChanPid]) ->
    {ok, ChanPid}.

handle_event({privmsg_addressed, {Source, <<"leave">>}}, ChanPid) ->
    handle_part(Source, ChanPid),
    {ok, ChanPid};
handle_event({privmsg_addressed, {Source, <<"part">>}}, ChanPid) ->
    handle_part(Source, ChanPid),
    {ok, ChanPid};
handle_event({privmsg_addressed, {Source, <<"go away">>}}, ChanPid) ->
    handle_part(Source, ChanPid),
    {ok, ChanPid};
handle_event({privmsg_addressed, {Source, <<"join ", ChanName/binary>>}}, ChanPid) ->
    ServPid = channel:get_server(ChanPid),
    server:join_channel(ServPid, ChanName),
    channel:add_op(server:get_channel(ServPid, ChanName), Source),
    {ok, ChanPid};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% ===================================================================
%% private functions
%% ===================================================================

handle_part(Source, ChanPid) ->
    case channel:is_op(ChanPid, Source) of
        true ->
            server:part_channel(channel:get_server(ChanPid), channel:get_name(ChanPid));
        false ->
            channel:send_message(ChanPid, {Source, <<"I'm afraid you can't do that.">>})
    end.
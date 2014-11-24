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

handle_event({privmsg_addressed, {Source, Text}}, ChanPid) ->
    ServPid = channel:get_server(ChanPid),
    case re:run(Text, "^(leave|part|go away)") of
        {match, _} ->
            case channel:is_op(ChanPid, Source) of
                true ->
                    server:part_channel(ServPid, channel:get_name(ChanPid));
                false ->
                    channel:send_message(ChanPid, {Source, <<"I'm afraid you can't do that.">>})
            end;
        _ ->
            []
    end,
    case re:run(Text, "^join (?<channel>.*)", [{capture, [1], binary}]) of
        {match, [ChanName]} ->
            server:join_channel(ServPid, ChanName),
            channel:add_op(server:get_channel(ServPid, ChanName), Source);
        _ ->
            []
    end,
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

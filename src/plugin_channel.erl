-module(plugin_channel).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([ChanPid]) ->
    {ok, ChanPid}.

handle_event({Source, Target, Text}, ChanPid) ->
    ServPid = channel:get_server(ChanPid),
    case server:get_nick(ServPid) of
        Target ->
            case re:run(Text, "leave|part|go away") of
                {match, _} ->
                    case channel:is_op(ChanPid, Source) of
                        true ->
                            server:part_channel(ServPid, channel:get_name(ChanPid));
                        false ->
                            ChanPid ! {privmsg, {Source, "I'm afraid you can't do that."}}
                    end;
                _ ->
                    []
            end,
            case re:run(Text, "^join (?<channel>.*)", [{capture, [1], list}]) of
                {match, [ChanName]} ->
                    server:join_channel(ServPid, ChanName),
                    channel:add_op(server:get_channel(ServPid, ChanName), Source);
                _ ->
                    []
            end;
        _ -> []
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
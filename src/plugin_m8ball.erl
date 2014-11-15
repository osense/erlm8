-module(plugin_m8ball).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([ChanPid]) ->
    {ok, ChanPid}.

handle_event({Source, Target, _Text}, ChanPid) ->
    BotNick = channel:get_nick(ChanPid),
    case Target of
        BotNick ->
            ChanPid ! {Source, "No."};
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

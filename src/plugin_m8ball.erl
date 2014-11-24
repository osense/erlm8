% This file is part of erlm8 released under the MIT license.
% See the LICENSE file for more information.

-module(plugin_m8ball).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([ChanPid]) ->
    {ok, ChanPid}.

handle_event({privmsg_addressed, {Source, Text}}, ChanPid) ->
    case re:run(Text, "^(am|are|you|is|can|would|will|does|do|could|should|shall|might) (?<question>.+)\?", [caseless, {capture, [2], binary}]) of
        {match, [Question]} ->
            channel:send_message(ChanPid, {Source, random_reply(Question)});
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


%% ===================================================================
%% private
%% ===================================================================

random_reply(Question) ->
    HashList = binary_to_list(crypto:hash(md5, Question)),
    lists:nth(1 + (lists:sum(HashList) rem length(random_replies())), random_replies()).

random_replies() ->
    [
        <<"quite possibly">>,
        <<"definitely maybe">>,
        <<"you are one cheeki kunt m8">>,
        <<"don't be ridiculous">>,
        <<"yes">>,
        <<"no">>,
        <<"it is certain">>,
        <<"there can only be one answer">>
    ].


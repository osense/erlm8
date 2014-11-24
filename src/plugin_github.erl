% This file is part of erlm8 released under the MIT license.
% See the LICENSE file for more information.

-module(plugin_github).
-behaviour(gen_event).
-compile(export_all).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([ChanPid]) ->
    MonPid = spawn_link(?MODULE, start_server, [self()]),
    {ok, {ChanPid, MonPid}}.

handle_event({privmsg_addressed, {_Source, Text}}, {ChanPid, MonPid}) ->
    case re:run(Text, "^github monitor (?<repo>.*)", [{capture, [1], binary}]) of
        {match, [RepoName]} ->
            MonPid ! {monitor, RepoName};
        _ ->
            []
    end,
    case re:run(Text, "^github demonitor (?<repo>.*)", [{capture, [1], binary}]) of
        {match, [DemonName]} ->
            MonPid ! {demonitor, DemonName};
        _ ->
            []
    end,
    case re:run(Text, "^github last (?<repo>.*)", [{capture, [1], binary}]) of
        {match, [LastName]} ->
            MonPid ! {last, LastName};
        _ ->
            []
    end,
    {ok, {ChanPid, MonPid}};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({repo_message, Name, Author, Message, Url}, {ChanPid, MonPid}) ->
    channel:send_message(ChanPid, <<"[github] ", Author/binary, " commited in ", Name/binary, ": ", Message/binary, " (", Url/binary, ")">>),
    {ok, ok, {ChanPid, MonPid}};
handle_call({repo_error, Name, Error}, {ChanPid, MonPid}) ->
    channel:send_message(ChanPid, <<"[github] ", Name/binary, ": ", Error/binary>>),
    {ok, ok, {ChanPid, MonPid}}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% ===================================================================
%% private functions
%% ===================================================================

start_server(HandlerPid) ->
    timer:send_interval(1000*60*30, update),
    loop(HandlerPid, orddict:new()).

loop(HandlerPid, RepoDict) ->
    receive
        {monitor, RepoName} ->
            case get_json(RepoName) of
                {ok, Json} ->
                    loop(HandlerPid, orddict:store(RepoName, Json, RepoDict));
                error ->
                    gen_event:call(HandlerPid, ?MODULE, {repo_error, RepoName, <<"error getting json">>})
            end;
        {demonitor, RepoName} ->
            case orddict:find(RepoName, RepoDict) of
                {ok, _} ->
                    loop(HandlerPid, orddict:erase(RepoName, RepoDict));
                error ->
                    gen_event:call(HandlerPid, ?MODULE, {repo_error, RepoName, <<"not monitored">>})
            end;
        {last, RepoName} ->
            case orddict:find(RepoName, RepoDict) of
                {ok, Json} ->
                    {Last} = lists:nth(1, Json),
                    print_commit(RepoName, Last, HandlerPid);
                error ->
                    gen_event:call(HandlerPid, ?MODULE, {repo_error, RepoName, <<"not monitored">>})
            end;
        update ->
            NewRepoDict = orddict:map(fun (Key, Val) -> update_repo(Key, Val, HandlerPid) end, RepoDict),
            loop(HandlerPid, NewRepoDict)
    end,
    loop(HandlerPid, RepoDict).


update_repo(Name, Json, HandlerPid) ->
    {Last} = lists:nth(1, Json),
    Sha = ej:get({"sha"}, Last),
    case get_json(Name) of
            {ok, NewJson} ->
                {NewLast} = lists:nth(1, NewJson),
                case ej:get({"sha"}, NewLast) of
                    undefined ->
                        log:info("plugin_github failed to parse json in ~p", [Name]);
                    Sha ->
                        [];
                    _NewSha ->
                        %% recursively print all the new commits in the correct (reverse) order
                        Fun = fun F(OldSha, CompleteJson, Index) ->
                            {SomeCommit} = lists:nth(Index, NewJson),
                            case ej:get({"sha"}, SomeCommit) of
                                OldSha ->
                                    [];
                                _SomeSha ->
                                    F(OldSha, CompleteJson, Index + 1),
                                    print_commit(Name, SomeCommit, HandlerPid)
                            end
                        end,
                        Fun(Sha, NewJson, 1)
                end,
                NewJson;
            error ->
                log:info("plugin_github failed to retrieve json in ~p", [Name]),
                Json
    end.

print_commit(RepoName, Json, HandlerPid) ->
    gen_event:call(HandlerPid, ?MODULE, {repo_message, RepoName,
        ej:get({"commit", "author", "name"}, Json),
        ej:get({"commit", "message"}, Json),
        ej:get({"html_url"}, Json)}).


% so this is pretty bad, but setting up a ssl http connection in erlang is even more so
get_json(Name) ->
    List = os:cmd("curl -s --raw https://api.github.com/repos/" ++ binary_to_list(Name) ++ "/commits"),
    try jiffy:decode(List) of
        % must be a list of commits
        Json when is_list(Json) ->
            {ok, Json};
        _ ->
            error
    catch
        _ ->
            error
    end.

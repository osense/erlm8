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

handle_event({_Source, Target, Text}, {ChanPid, MonPid}) ->
    ServPid = channel:get_server(ChanPid),
    case server:get_nick(ServPid) of
        Target ->
            case re:run(Text, "^github monitor (?<repo>.*)", [{capture, [1], list}]) of
                {match, [RepoName]} ->
                    MonPid ! {monitor, RepoName};
                _ ->
                    []
            end,
            case re:run(Text, "^github demonitor (?<repo>.*)", [{capture, [1], list}]) of
                {match, [DemonName]} ->
                    MonPid ! {demonitor, DemonName};
                _ ->
                    []
            end,
            case re:run(Text, "^github last (?<repo>.*)", [{capture, [1], list}]) of
                {match, [LastName]} ->
                    MonPid ! {last, LastName};
                _ ->
                    []
            end;
        _ -> []
    end,
    {ok, {ChanPid, MonPid}};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({repo_message, Name, Author, Message, Url}, {ChanPid, MonPid}) ->
    ChanPid ! {privmsg, io_lib:format("[github] ~p commited in ~p: ~p (~p)", [Author, Name, Message, Url])},
    {ok, ok, {ChanPid, MonPid}};
handle_call({repo_error, Name, Error}, {ChanPid, MonPid}) ->
    ChanPid ! {privmsg, io_lib:format("[github] ~p: ~p", [Name, Error])},
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
    Pid = self(),
    spawn_link(fun F() -> Pid ! update, timer:sleep(1000*60*30), F() end),
    loop(HandlerPid, orddict:new()).

loop(HandlerPid, RepoDict) ->
    receive
        {monitor, RepoName} ->
            case get_json(RepoName) of
                {ok, Json} ->
                    loop(HandlerPid, orddict:store(RepoName, Json, RepoDict));
                error ->
                    gen_event:call(HandlerPid, ?MODULE, {repo_error, RepoName, "error getting json"})
            end;
        {demonitor, RepoName} ->
            case orddict:find(RepoName, RepoDict) of
                {ok, _} ->
                    loop(HandlerPid, orddict:erase(RepoName, RepoDict));
                error ->
                    gen_event:call(HandlerPid, ?MODULE, {repo_error, RepoName, "not monitored"})
            end;
        {last, RepoName} ->
            case orddict:find(RepoName, RepoDict) of
                {ok, Json} ->
                    {Last} = lists:nth(1, Json),
                    print_commit(RepoName, Last, HandlerPid);
                error ->
                    gen_event:call(HandlerPid, ?MODULE, {repo_error, RepoName, "not monitored"})
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
                {NewLast} = lists:nth(1, Json),
                case ej:get({"sha"}, NewLast) of
                    undefined ->
                        gen_event:call(HandlerPid, ?MODULE, {repo_error, Name, "error parsing json"});
                    Sha ->
                        Sha;
                    _NewSha ->
                        % recursively print all the new commits in the correct (reverse) order
                        Fun = fun F(OldSha, CompleteJson, Index) ->
                            {SomeCommit} = lists:nth(Index, Json),
                            case ej:get({"sha"}, SomeCommit) of
                                OldSha ->
                                    [];
                                _SomeSha ->
                                    F(OldSha, CompleteJson, Index + 1),
                                    print_commit(Name, SomeCommit, HandlerPid)
                            end
                        end,
                        Fun(Sha, Json, 1)
                end,
                NewJson;
            error ->
                gen_event:call(HandlerPid, ?MODULE, {repo_error, Name, "error getting json"}),
                Json
    end.

print_commit(RepoName, Json, HandlerPid) ->
    gen_event:call(HandlerPid, ?MODULE, {repo_message, RepoName,
        binary_to_list(ej:get({"commit", "author", "name"}, Json)),
        binary_to_list(ej:get({"commit", "message"}, Json)),
        binary_to_list(ej:get({"html_url"}, Json))}).


% so this is pretty bad, but setting up a ssl http connection in erlang is even more so
get_json(Name) ->
    List = os:cmd("curl -s --raw https://api.github.com/repos/" ++ Name ++ "/commits"),
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

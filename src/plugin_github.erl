-module(plugin_github).
-behaviour(gen_event).
-compile(export_all).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%% ===================================================================
%% gen_event callbacks
%% ===================================================================

init([ChanPid]) ->
    MonPid = spawn_link(?MODULE, loop, [self(), []]),
    {ok, {ChanPid, MonPid}}.

handle_event({Source, Target, Text}, {ChanPid, MonPid}) ->
    ServPid = channel:get_server(ChanPid),
    case server:get_nick(ServPid) of
        Target ->
            case re:run(Text, "^monitor (?<repo>.*)", [{capture, [1], list}]) of
                {match, [Repo]} ->
                    MonPid ! {monitor, Repo};
                _ ->
                    []
            end;
        _ -> []
    end,
    {ok, {ChanPid, MonPid}};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({github_repo_change, Name, Author, Message, Url}, {ChanPid, MonPid}) ->
    ChanPid ! {privmsg, io_lib:format("[~p commited in ~p]: ~p (~p)",
                        [Author, Name, re:replace(Message, "(\n)*", " ", [global, {return, list}]), Url])},
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

loop(HandlerPid, Repos) ->
    receive
        {monitor, Repo} ->
            loop(HandlerPid, [{Repo, ""} | Repos])
    after 10000 ->
        NewRepos = check_repos([], Repos, HandlerPid),
        loop(HandlerPid, NewRepos)
    end,
    loop(HandlerPid, Repos).

check_repos(NewRepos, [], _) ->
    NewRepos;
check_repos(NewRepos, [{Name, LastSha} | Tail], HandlerPid) ->
    {Sha, Author, Message, Url} = get_nth_commit(1, get_json(Name)),
    case Sha of
        LastSha ->
            check_repos([{Name, LastSha} | NewRepos], Tail, HandlerPid);
        _ ->
            gen_event:call(HandlerPid, ?MODULE, {github_repo_change, Name, Author, Message, Url}),
            check_repos([{Name, Sha} | NewRepos], Tail, HandlerPid)
    end.

% so this is pretty bad, but setting up a ssl http connection in erlang is even more so
get_json(Name) ->
    jiffy:decode(os:cmd("curl -s --raw https://api.github.com/repos/" ++ Name ++ "/commits")).

get_nth_commit(Index, Json) ->
    {Commit} = lists:nth(Index, Json),
    {
        ej:get({"sha"}, Commit),
        binary_to_list(ej:get({"commit", "author", "name"}, Commit)),
        binary_to_list(ej:get({"commit", "message"}, Commit)),
        binary_to_list(ej:get({"html_url"}, Commit))
    }.

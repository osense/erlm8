-module(server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_server/2, kill_server/1, get_server/1]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_server(Serv, Port) ->
    log:log("starting server " ++ Serv ++ " on port " ++ integer_to_list(Port)),
    {ok, Pid} = supervisor:start_child(?MODULE, [Serv, Port]),
    Pid.

kill_server(Name) ->
    lists:map(fun({_, Child, _, _}) ->
        case server:get_name(Child) of
            Name ->
                ok = supervisor:terminate_child(?MODULE, Child),
                log:log("killed server " ++ Name);
            _ ->
                []
        end
    end, supervisor:which_children(?MODULE)).

get_server(Index) ->
    [H | Tail] = supervisor:which_children(?MODULE),
    {_, Child, _, _} = H,
    Child.


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 1, 5},
          [{server, {server, start_link, []},
            transient, 5000, worker, [server]}]}}.


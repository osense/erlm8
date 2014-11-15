-module(log).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/1, set_debug/1, info/1, info/2, debug/1, debug/2]).

-record(state, {
    filename,
    debug_mode}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename], []).

set_debug(Debug) when is_boolean(Debug) ->
    gen_server:call(?MODULE, {set_debug, Debug}).

info(Text) ->
    gen_server:cast(?MODULE, {info, Text}).

info(Text, Args) ->
    gen_server:cast(?MODULE, {info, io_lib:format(Text, Args)}).

debug(Text) ->
    gen_server:cast(?MODULE, {debug, Text}).

debug(Text, Args) ->
    gen_server:cast(?MODULE, {debug, io_lib:format(Text, Args)}).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Filename]) ->
    file:write_file(Filename, "opened log\n"),
    {ok, #state {
        filename = Filename,
        debug_mode = false}}.

handle_call({set_debug, Debug}, _From, State) ->
    case Debug of
        true ->
            info("enabled debug logging");
        _ ->
            info("disabled debug logging")
    end,
    {reply, ok, State#state{debug_mode = Debug}}.

handle_cast({info, Text}, State = #state {filename = File}) ->
    file:write_file(File, Text ++ "\n", [append]),
    {noreply, State};
handle_cast({debug, Text}, State = #state {filename = File, debug_mode = Debug}) ->
    case Debug of
        true ->
            file:write_file(File, Text ++ "\n", [append]);
        _ -> []
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {shutdown, Reason}.

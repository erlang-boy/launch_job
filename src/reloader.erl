%%% =================================================================
%%% @author Liu HuiDong
%%% @date   16-6-18
%%% @copyright huidong.liu@qingteng.me
%%% @doc @todo Add description to reloader
%%% =================================================================

-module(reloader).
-author("Liu HuiDong").

%%% =================================================================
%%% API functions
%%% =================================================================

-include_lib("kernel/include/file.hrl").

-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-export([start/0, start_link/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([reload_all/0, test_all/0]).

-record(state, {last, tref}).

%% External API
%%% -----------------------------------------------------------------
%%% start/0
%%% -----------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%%% -----------------------------------------------------------------
%%% start_link/0
%%% -----------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% -----------------------------------------------------------------
%%% stop/0
%%% -----------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).

%%% -----------------------------------------------------------------
%%% reload_all/0
%%% -----------------------------------------------------------------
reload_all() ->
    Now = stamp(),
    _ = doit(0, Now).

%%% -----------------------------------------------------------------
%%% test_all/0
%%% -----------------------------------------------------------------
test_all() ->
    eunit:test({dir, "ebin"}, [verbose]).

%%% =================================================================
%%% gen_server callbacks
%%% =================================================================
init([]) ->
    {ok, TRef} = timer:send_interval(timer:seconds(1), doit),
    {ok, #state{last = stamp(), tref = TRef}}.

handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(doit, State) ->
    Now = stamp(),
    _ = doit(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%% =================================================================
%%% Internal API
%%% =================================================================
module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn;
module_vsn(M) when is_atom(M) ->
    module_vsn(M:module_info()).

sources() ->
    ServerPath = os:getenv("SERVERPATH"),
    Path = filename:join([ServerPath, "src/**/*.?rl"]),
    sources(Path).
sources(Path) ->
    filelib:wildcard(Path).

is_changed(Source, From, To) ->
    case file:read_file_info(Source) of
        {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
            true;
        {ok, _} ->
            false;
        {error, Reason} ->
            {error, Reason}
    end.

doit(From, To) ->
    [case is_changed(Filename, From, To) of
         true ->
             compile(Filename);
         false ->
             unmodified;
         {error, Reason} ->
             lager:info("Error reading ~s: ~p~n", [Filename, Reason]),
             error
     end || Filename <- sources()].

compile(Filename) ->
    lager:info("Compiling ~p ...~n", [Filename]),
    OutDir = get_outdir(Filename),
    case compile:file(Filename, [{outdir, OutDir}, {i, "include"}, debug_info, report]) of
        {ok, Module} ->
            lager:info("succeeded (~p)~n", [Module]),
            reload(Module);
        _ ->
            lager:info("failed (~p)~n", [Filename])
    end.


reload(Module) ->
    lager:info("Reloading ~p ... ", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            lager:info("version ~w ok.~n", module_vsn(Module));
        {error, Reason} ->

            lager:info(" fail: ~p.~n", [Reason]),
            error
    end.

get_outdir(Filename) ->
    BaseName = filename:basename(Filename, ".erl"),
    FilePath = code:which(list_to_atom(BaseName)),
    OutDir = filename:dirname(FilePath),
    OutDir.

stamp() ->
    erlang:localtime().
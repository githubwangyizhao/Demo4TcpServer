-module(tcp_server_app).

-behaviour(application).

%% Application callbacks
-export([ensure_start/0]).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

ensure_start() ->
    ensure_deps_start([tcp_server]).

start(_StartType, _StartArgs) ->
    tcp_server_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ensure_deps_start([]) ->
    ok;
ensure_deps_start([App | T] = Apps) ->
    case application:start(App) of
        {error, {not_started, App2}} ->
            add_deps_path(App2),
            ensure_deps_start([App2] ++ Apps);
        {error, Reason} ->
            io:format("start application ~p failed, reason: ~p~n", [App, Reason]);
        _ ->
            io:format("application ~p start success. ~n", [App]),
            ensure_deps_start(T)
    end.

add_deps_path(App) ->
    code:add_path(lists:concat(["../deps/", App, "/ebin"])).



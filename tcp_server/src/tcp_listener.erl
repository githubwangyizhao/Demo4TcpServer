%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 七月 2019 11:24
%%%-------------------------------------------------------------------
-module(tcp_listener).
-author("Administrator").

-behaviour(gen_server).
-include("logger.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(DEF_PORT, 2333).

-record(state, {
    listener,  %% Listening socket
    acceptor   %% Asynchronous acceptor's internal reference
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    Opts = [binary,
        {packet, 2},
        {reuseaddr, true},
        {keepalive, true},
        {backlog, 30},
        {active, false}    %% 设置套接字为被动模式。套接字收到的消息被缓存起来。
    ],
    Port = get_app_env(listen_port, ?DEF_PORT),
    %% 开启一个监听某个端口的套接字 
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            ?INFO_LOG("listen port: ~p. ", [Port]),
            {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
            {ok, #state{listener = ListenSocket, acceptor = Ref}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({inet_async, ListenSocket, Ref, {ok, ClientSocket}}, #state{listener = ListenSocket, acceptor = Ref} = State) ->
    try
        case set_sockopt(ListenSocket, ClientSocket) of
            ok -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,
        {ok, Pid} = tcp_echo_statem:start_client(),
        gen_tcp:controlling_process(ClientSocket, Pid),     %% 为socket分配一个新的控制进程id
        tcp_echo_statem:set_socket(Pid, ClientSocket),
        case prim_inet:async_accept(ListenSocket, 1) of
            {ok, NewRef} -> {noreply, State#state{acceptor = NewRef}};
            {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end
    catch exit:Why ->
        ?ERROR_LOG("Error in async accept: ~p.", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, ListenSocket, Ref, Error}, #state{listener = ListenSocket, acceptor = Ref} = State) ->
    ?ERROR_LOG("Error in socket acceptor: ~p. ", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_app_env(Opt, Default) ->
    {ok, App} = application:get_application(),
    case application:get_env(App, Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                {ok, [[Val | _]]} -> list_to_integer(Val);
                error -> Default
            end
    end.

set_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(ClientSocket, Opts) of
                ok -> ok;
                Error -> gen_tcp:close(ClientSocket), Error
            end;
        Error ->
            gen_tcp:close(ClientSocket), Error
    end.

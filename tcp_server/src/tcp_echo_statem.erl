%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 七月 2019 11:56
%%%-------------------------------------------------------------------
-module(tcp_echo_statem).
-author("Administrator").

-behaviour(gen_statem).
-include("logger.hrl").

%% API
-export([start_link/0]).
-export([set_socket/2, start_client/0]).

%% gen_statem callbacks
-export([
    init/1,
    format_status/2,
    state_name/3,
    handle_event/4,
    terminate/3,
    code_change/4,
    callback_mode/0
]).

-define(TIMEOUT, 120000).
-define(SERVER, ?MODULE).

-record(state, {
    socket,         %% Client socket
    addr            %% Client address
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_statem:cast(Pid, {socket_ready, Socket}).

start_client() ->
    supervisor:start_child(tcp_client_sup, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #state{}}.

callback_mode() ->
    handle_event_function.

format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

state_name(_EventType, _EventContent, State) ->
    NextStateName = next_state,
    {next_state, NextStateName, State}.

handle_event(cast, {socket_ready, Socket}, wait_for_socket, State) ->
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),        %% 将套接字设置为主动模式，但是一旦收到第一条消息，就将其设置为被动模式
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, wait_for_data, State#state{socket = Socket, addr = IP}, {timeout, ?TIMEOUT, {conn_timeout}}};

handle_event(timeout, {conn_timeout}, wait_for_data, State) ->
    ?ERROR_LOG("~p client connection timeout - closing. ", [self()]),
    {stop, normal, State};

handle_event(info, {tcp, Socket, Bin}, wait_for_data, #state{socket = Socket} = State) ->
    ?INFO_LOG("~p receive data: ~p ", [self(), Bin]),
    inet:setopts(Socket, [{active, once}]),
    ok = gen_tcp:send(Socket, Bin),
    {keep_state, State, {timeout, ?TIMEOUT, {conn_timeout}}};

handle_event(info, {tcp_closed, Socket}, _StateName, #state{socket = Socket, addr = Addr} = State) ->
    ?INFO_LOG("~p client ~p disconnected. ", [self(), Addr]),
    {stop, normal, State};

handle_event(info, _Event, _, State) ->
    {stop, normal, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

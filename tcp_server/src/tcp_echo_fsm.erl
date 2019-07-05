%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%    已弃用，tcp_echo_fsm 模块替换成 tcp_echo_statem 模块
%%% @end
%%% Created : 04. 七月 2019 15:00
%%%-------------------------------------------------------------------
-module(tcp_echo_fsm).
-author("Administrator").

-behaviour(gen_fsm).
-include("logger.hrl").

%% API
-export([start_link/0, set_socket/2, start_client/0]).

%% gen_fsm callbacks
-export([init/1,
    wait_for_socket/2,
    wait_for_data/2,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 120000).

-record(state, {
    socket,         %% Client socket
    addr            %% Client address
}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

start_client() ->
    supervisor:start_child(tcp_client_sup, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, wait_for_socket, #state{}}.

wait_for_socket({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, wait_for_data, State#state{socket = Socket, addr = IP}, ?TIMEOUT};
wait_for_socket(Other, State) ->
    ?ERROR_LOG("State: wait_for_socket Unexpected  message: ~p ", [Other]),
    {next_state, wait_for_socket, State}.

wait_for_data({data, Data}, #state{socket = S} = State) ->
    ?INFO_LOG("~p receive data: ~p ", [self(), Data]),
    ok = gen_tcp:send(S, Data),
    {next_state, wait_for_data, State, ?TIMEOUT};
wait_for_data(timeout, State) ->
    ?ERROR_LOG("~p Client connection timeout - closing. ", [self()]),
    {stop, normal, State};
wait_for_data(Data, State) ->
    ?INFO_LOG("~p Ignoring data: ~p ", [self(), Data]),
    {next_state, wait_for_data, State, ?TIMEOUT}.
    
handle_event(Event, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(Event, _From, StateName, State) ->
    {stop, {StateName, undefined_event, Event}, State}.

handle_info({tcp, Socket, Bin}, StateName, #state{socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, State);
handle_info({tcp_closed, Socket}, _StateName, #state{socket = Socket, addr = Addr} = State) ->
    ?INFO_LOG("~p Client ~p disconnected. ", [self(), Addr]),
    {stop, normal, State};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

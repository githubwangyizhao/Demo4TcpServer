%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 七月 2019 17:44
%%%-------------------------------------------------------------------
-module(test_gateway).
-author("Administrator").

%% API
-export([connect/0, connect/1, close/1, request/2]).

connect() -> connect(true).
connect(Close) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 443, [binary, {packet, 2}]),
    case Close of
        false -> Socket;
        true ->
            request(Socket, <<"hello">>),
            request(Socket, <<"world">>),
            request(Socket, <<"!!">>),
            close(Socket)
    end.

close(Socket) ->
    gen_tcp:close(Socket).

request(Socket, Msg) ->
    ok = gen_tcp:send(Socket, Msg),
    receive_msg().

receive_msg() ->
    receive M ->
        error_logger:info_msg("~p~n", [M])
    end.
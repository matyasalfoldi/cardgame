%% tcp_server.erl
-module(tcp_server).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(8000, [binary, {packet, line}, {active, false}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle(Socket) end),
    accept(ListenSocket).

handle(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"play_card 3\n">>} ->
            gen_tcp:send(Socket, <<"ok: card played\n">>);
        {ok, Data} ->
            gen_tcp:send(Socket, <<"unknown command: ", Data/binary>>);
        {error, enotconn} ->
            ok;
        {error, closed} ->
            ok
    end,
    handle(Socket).

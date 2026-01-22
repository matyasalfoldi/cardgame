%% tcp_server.erl
-module(tcp_server).
-export([start/0]).

start() ->
    {ok, ListenSocket} =
        gen_tcp:listen(8000, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
    io:format("Waiting for 2 players...~n"),
    {ok, P1} = gen_tcp:accept(ListenSocket),
    io:format("Player 1 connected~n"),
    {ok, P2} = gen_tcp:accept(ListenSocket),
    io:format("Player 2 connected~n"),
    game_loop(P1, P2).

%% ---------------------------------------------------------
%% GAME LOOP
%% ---------------------------------------------------------
game_loop(P1, P2) ->
    Cmd1 = wait_for_command(P1),
    Cmd2 = wait_for_command(P2),

    case {Cmd1, Cmd2} of
        {game_ended, game_ended} ->
            io:format("Both players ended the game~n"),
            gen_tcp:send(P1, <<"game_over\n">>),
            gen_tcp:send(P2, <<"game_over\n">>),
            ok;
        
        {game_over, _} ->
            io:format("Player 1 ended the game~n"),
            gen_tcp:send(P2, <<"game_over\n">>),
            ok;

        {_, game_over} ->
            io:format("Player 2 ended the game~n"),
            gen_tcp:send(P1, <<"game_over\n">>),
            ok;

        {{play_card, C1}, {play_card, C2}} ->
            io:format("Both played cards: ~p and ~p~n", [C1, C2]),

            case C1 =:= C2 of
                true ->
                    gen_tcp:send(P1, <<"draw\n">>),
                    gen_tcp:send(P2, <<"draw\n">>);
                false when C1 > C2 ->
                    gen_tcp:send(P1, <<"win\n">>),
                    gen_tcp:send(P2, <<"lose\n">>);
                false ->
                    gen_tcp:send(P1, <<"lose\n">>),
                    gen_tcp:send(P2, <<"win\n">>)
            end,

            game_loop(P1, P2);

        _ ->
            io:format("Unexpected command(s): ~p ~p~n", [Cmd1, Cmd2]),
            game_loop(P1, P2)
    end.

%% ---------------------------------------------------------
%% RECEIVE A SINGLE COMMAND FROM ONE PLAYER
%% ---------------------------------------------------------
wait_for_command(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"play_card ", Rest/binary>>} ->
            case binary:split(Rest, <<"\n">>, [global]) of
                [NumBin, _] ->
                    {play_card, binary_to_integer(NumBin)}
            end;

        {ok, <<"game_ended\n">>} ->
            game_ended;

        {ok, Other} ->
            io:format("Unknown command: ~p~n", [Other]),
            wait_for_command(Socket);

        {error, closed} ->
            io:format("Client disconnected~n"),
            game_ended
    end.

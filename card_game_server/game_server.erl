%% game_server.erl
-module(game_server).
-behaviour(gen_server).

%% API
-export([start_link/2, play_card/2, get_score/1]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {player1, player2, score1=0, score2=0}).

%% Start a new game
start_link(P1, P2) ->
    gen_server:start_link(?MODULE, {P1, P2}, []).

init({P1, P2}) ->
    {ok, #state{player1=P1, player2=P2}}.

%% Player plays a card
play_card(Server, {Player, Card}) ->
    gen_server:cast(Server, {play, Player, Card}).

handle_cast({play, Player, Card}, State) ->
    %% Simplified: assume both players send one card each round
    NewState = resolve_round(Player, Card, State),
    {noreply, NewState}.

%% Compare cards and update scores
resolve_round(Player, Card, State=#state{score1=S1, score2=S2}) ->
    %% Example: compare attack values
    case Card of
        {attack, A} ->
            %% Fake opponent card for demo
            OpponentCard = {attack, 5},
            case A > 5 of
                true -> State#state{score1=S1+1};
                false when A < 5 -> State#state{score2=S2+1};
                false -> State#state{score1=S1+1, score2=S2+1}
            end
    end.

get_score(Server) ->
    gen_server:call(Server, get_score).

handle_call(get_score, _From, State=#state{score1=S1, score2=S2}) ->
    {reply, {S1, S2}, State}.

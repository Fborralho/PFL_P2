% Initial Game Setup

init_game_state(gameState(Board, CurrentPlayer, CapturedPieces, Config)):-
    initial_board(Board),
    CurrentPlayer = black,
    CapturedPieces = captured_pieces(0, 0),
    Config = [].

get_board(gameState(Board,_, _ , _), Board).

get_currentPlayer(gameState(_, CurrentPlayer, _, _), CurrentPlayer).

get_capturedPieces(gameState(_, _, CapturedPieces, _), CapturedPieces).

get_config(gameState(_, _, _, Config), Config).


% Test Display 
initial_board([
    [b, b, b, b, b, b, empty, empty],
    [b, b, b, b, b, b, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, w, w],
    [empty, empty, empty, empty, empty, empty, w, w]
]).

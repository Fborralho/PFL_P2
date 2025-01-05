:- ensure_loaded('game_state.pl').
:- use_module(library(lists)).

% valid moves for each player
valid_non_capturing_move(Player, FromRow, FromCol, ToRow, ToCol, Board) :-
    (Player = black -> Directions = [(1,0), (1,1), (0,1), (-1,1)];
     Player = white -> Directions = [(0,1), (-1,1), (-1,0), (-1,-1)]),
    member((DR, DC), Directions),
    ToRow is FromRow + DR,
    ToCol is FromCol + DC,
    within_board(ToRow, ToCol),
    piece_at(Board, ToRow, ToCol, empty).  % Ensure the destination cell is empty


% Valid capturing moves for both players

valid_capturing_move(Player, FromRow, FromCol, ToRow, ToCol, Board):-
    Directions = [(1, 1), (1, -1), (-1, -1), (-1, 1)], % Only diagonal moves allowed
    member((DR,DC), Directions),
    between(1,8, Step),
    ToRow is FromRow + DR * Step,
    ToCol is FromCol + DC * Step,                          % Checks if move is diagonal and if there is an opponent piece in line to capture
    within_board(ToRow, ToCol),                         
    piece_at(Board, ToRow, ToCol, OpponentPiece),
    OpponentPiece \= empty,
    is_opponent(Player, OpponentPiece),
    is_path_clear(Board, FromRow, FromCol, ToRow, ToCol). % Check if there is a piece in the way.


% Check if that piece belongs to the opponent

is_opponent(black, w).
is_opponent(white, b).

% Between funcion to iterate through the steps
between(Low, High, Low) :- Low =< High.
between(Low, High, Value) :-
    Low < High,
    NextLow is Low + 1,
    between(NextLow, High, Value).


% Checks if the position is within the board

within_board(Row, Col):-
    Row >= 1, Row =< 8,
    Col >= 1, Col =< 8.


% Get the piece at specific position

piece_at(Board, Row, Col, Piece):-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece).


is_path_clear(Board, FromRow, FromCol, ToRow, ToCol) :-
    DeltaRow is ToRow - FromRow,
    DeltaCol is ToCol - FromCol,
    StepRow is sign(DeltaRow), % Determine the step direction for rows
    StepCol is sign(DeltaCol), % Determine the step direction for columns
    check_path(Board, FromRow, FromCol, StepRow, StepCol, ToRow, ToCol).

% Recursively checks the path up to but not including destionation square
check_path(Board, FromRow, FromCol, StepRow, StepCol, ToRow, ToCol) :-
    (ToRow > FromRow -> StepRow = 1; ToRow < FromRow -> StepRow = -1; StepRow = 0),
    (ToCol > FromCol -> StepCol = 1; ToCol < FromCol -> StepCol = -1; StepCol = 0),
    
    NextRow is FromRow + StepRow,
    NextCol is FromCol + StepCol,
    
    (NextRow =:= ToRow, NextCol =:= ToCol -> true; 
        piece_at(Board, NextRow, NextCol, empty),
        check_path(Board, NextRow, NextCol, StepRow, StepCol, ToRow, ToCol)
    ).



% Gets a list of the valid moves

valid_moves(Player, Board, Moves):-
    findall(
        (FromRow, FromCol, ToRow, ToCol),
        (
            piece_at(Board, FromRow, FromCol, PlayerPiece),  % Get piece coordinates
            is_player_piece(Player, PlayerPiece),  % Ensure its the current players piece
            is_valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board)  % Check valid move
        ),
        Moves
    ).


% Checks if piece belongs to that player.

is_player_piece(black, b).
is_player_piece(white, w).

is_valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board):-
    (valid_non_capturing_move(Player, FromRow, FromCol, ToRow, ToCol, Board);
     valid_capturing_move(Player, FromRow, FromCol, ToRow, ToCol, Board)).


% Execute a move and return the new game state

move(GameState, (FromRow, FromCol, ToRow, ToCol), NewGameState) :-
    get_board(GameState, Board),
    get_currentPlayer(GameState, Player),
    (valid_capturing_move(Player, FromRow, FromCol, ToRow, ToCol, Board)->
        piece_at(Board,ToRow,ToCol,CapturedPiece),
        update_captured_pieces(Player, CapturedPiece, GameState, UpdatedCapturedPieces);
        get_capturedPieces(GameState, UpdatedCapturedPieces)),
    execute_move(Board, FromRow, FromCol, ToRow, ToCol, UpdatedBoard),
    switch_player(Player, NextPlayer),
    NewGameState = gameState(UpdatedBoard, NextPlayer, UpdatedCapturedPieces, _).  

% Pass turn

switch_player(black, white).
switch_player(white, black).

% Update board after a move
execute_move(Board, FromRow, FromCol, ToRow, ToCol, UpdatedBoard) :-
    piece_at(Board, FromRow, FromCol, Piece),
    set_piece(Board, FromRow, FromCol, empty, TempBoard),  % Remove piece from original position
    set_piece(TempBoard, ToRow, ToCol, Piece, UpdatedBoard).  % Place piece in the new position

% Update the piece at a specific position
set_piece(Board, Row, Col, NewPiece, UpdatedBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(OldRow, Col, NewPiece, UpdatedRow),
    replace_nth(Board, Row, UpdatedRow, UpdatedBoard).

% Replace the Nth element in a list
replace_nth([_|T], 1, X, [X|T]).
replace_nth([H|T], N, X, [H|R]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(T, N1, X, R).

update_captured_pieces(Player, CapturedPiece, GameState, UpdatedCapturedPieces):-
    get_capturedPieces(GameState,captured_pieces(BlackCount, WhiteCount)),
    (Player = black, CapturedPiece = w ->
        NewBlackCount is BlackCount + 1,
        UpdatedCapturedPieces = captured_pieces(NewBlackCount, WhiteCount);
     Player = white, CapturedPiece = b ->
        NewWhiteCount is WhiteCount + 1,
        UpdatedCapturedPieces = captured_pieces(BlackCount, NewWhiteCount);
     UpdatedCapturedPieces = captured_pieces(BlackCount,WhiteCount)).


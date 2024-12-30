:- ensure_loaded('game_state.pl').

% Valid non capturing moves for black

valid_non_capturing_move(black, FromRow, FromCol, ToRow, ToCol):-
    Directions = [(1,0), (1,1), (0,1), (-1,1)], % Black has chess king non capturing moves to N , NE, E and SE. 
    member((DR,DC), Directions),
    ToRow is FromRow + DR,
    ToCol is FromCol + DC,
    within_board(ToRow, ToCol).

% Valid non capturing moves for white
valid_non_capturing_move(white, FromRow, FromCol, ToRow, ToCol) :-
    Directions = [(0, 1), (-1, 1), (-1, 0), (-1, -1)], % White has chess king non capturing moves to E, NE, N, NW. (Coor from Black Perspective)
    member((DR, DC), Directions),
    ToRow is FromRow + DR,
    ToCol is FromCol + DC,
    within_board(ToRow, ToCol).

% Valid capturing moves for both players

valid_capturing_move(Player, FromRow, FromCol, ToRow, ToCol, Board):-
    Directions = [(1, 1), (1, -1), (-1, -1), (-1, 1)], % Only diagonal moves allowed
    member((DR,DC), Directions),
    ToRow is FromRow + DR,
    ToCol is FromCol + DC,                          % Checks if move is diagonal and if there is an opponent piece in line to capture
    within_board(ToRow, ToCol),                         
    piece_at(Board, ToRow, ToCol, OpponentPiece),
    OpponentPiece \= empty,
    is_opponent(Player, OpponentPiece).



% Check if that piece belongs to the opponent

is_opponent(Black, w).
is_opponent(White, b).

% Checks if the position is within the board

within_board(Row, Col):-
    Row >= 1, Row =< 8,
    Col >= 1, Col =< 8.


% Get the piece at specific position

piece_at(Board, Row, Col, Piece):-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece).

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

is_player_piece(Black, b).
is_player_piece(White, w).

is_valid_move(Player, FromRow, FromCol, ToRow, ToCol, Board):-
    (valid_non_capturing_move(Player, FromRow, FromCol, ToRow, ToCol);
     valid_capturing_move(Player, FromRow, FromCol, ToRow, ToCol, Board)).





test_valid_moves :-
    initial_board(Board),
    valid_moves(black, Board, Moves),
    write(Moves), nl.
:- ensure_loaded('game_state.pl')

% Valid non capturing moves for black

valid_non_capturing_move(black, fromRow, fromCol, toRow, toCol):-
    Directions = [(1,0), (1,1), (0,1), (-1,1)], % Black has chess king non capturing moves to N , NE, E and SE. 
    member((DR,DC), Directions),
    toRow is fromRow + DR,
    toCol is fromCol + DC,
    within_board(toRow, toCol).

% Valid non capturing moves for white
valid_non_capturing_move(white, FromRow, FromCol, ToRow, ToCol) :-
    Directions = [(0, 1), (-1, 1), (-1, 0), (-1, -1)], % White has chess king non capturing moves to E, NE, N, NW. (Coor from Black Perspective)
    member((DR, DC), Directions),
    ToRow is FromRow + DR,
    ToCol is FromCol + DC,
    within_board(ToRow, ToCol).

% Valid capturing moves for both players

valid_capturing_move(Player, fromRow, fromCol, toRow, toCol, Board):-
    Directions = [(1, 1), (1, -1), (-1, -1), (-1, 1)], % Only diagonal moves allowed
    member((DR,DC), Directions),
    toRow is fromRow + DR,
    toCol is fromCol + DC,                          % Checks if move is diagonal and if there is an opponent piece in line to capture
    within_board(toRow, toCol),                         
    piece_at(Board, toRow, toCol, OpponentPiece),
    OpponentPiece \= empty,
    is_opponent(Player, OpponentPiece).



% Check if that piece belongs to the opponent

is_opponent(Black, 'W').
is_opponent(White, 'B').

% Checks if the position is within the board

within_board(Row, Col):-
    Row >= 1, Row <= 8,
    Col >= 1, Col <= 8.


% Get the piece at specific position

piece_at(Board, Row, Col, Piece):-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Piece).

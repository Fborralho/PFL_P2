:-ensure_loaded('game_state.pl').

% Display the game state.

display_game(GameState):- 
    get_board(GameState, Board),
    get_currentPlayer(GameState,Player),
    get_capturedPieces(GameState,captured_pieces(BlackCount, WhiteCount)),
    nl, write(Player), write(' turn to play.'), nl,
    nl, write('  A B C D E F G H'), nl,
    display_rows(Board, 8),             % Extracts the board from gamestate, write the columns and rows as letters 
    nl,                                 % and numbers, respectively.
    display_score(BlackCount, WhiteCount, CurrentPlayer).


% Display Function 

display_rows([],0).
display_rows([Row|Rows], RowNum):-
    write(RowNum), write(' '), 
    display_row(Row),
    nl,                                % Goes through the rows and writes the content then moves to the next line.
    NextRowNum is RowNum - 1,
    display_rows(Rows, NextRowNum).


display_row([]).
display_row([Cell|Cells]):-
    display_cell(Cell),                % Adds a space between each cell in a row
    write(' '),
    display_row(Cells).

% Display functions to each type of cell 

display_cell(b):- 
    write('B').      % Display Black Piece

display_cell(w):-                      
    write('W').      % Display White Piece

display_cell(empty):-
    write('.').      % Display empty Piece

display_score(BlackCount, WhiteCount, CurrentPlayer):-
    format('Score: Black ~d - White ~d~n', [BlackCount, WhiteCount]),
    (
        BlackCount > WhiteCount, CurrentPlayer = black ->
            format('You\'re winning ~d to ~d!~n', [BlackCount, WhiteCount]);
        BlackCount > WhiteCount, CurrentPlayer = white ->
            format('You\'re losing ~d to ~d.~n', [WhiteCount, BlackCount]);
        BlackCount < WhiteCount, CurrentPlayer = black ->
            format('You\'re losing ~d to ~d.~n', [WhiteCount, BlackCount]);
        BlackCount < WhiteCount, CurrentPlayer = white ->
            format('You\'re winning ~d to ~d!~n', [WhiteCount, BlackCount]);
        BlackCount =:= WhiteCount ->
            format('The game is tied ~d - ~d.~n', [BlackCount, WhiteCount])).

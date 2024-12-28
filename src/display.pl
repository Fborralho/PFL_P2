% Display the game state.

display_game(Board):-    
    nl, write('  A B C D E F G H'), nl,
    display_rows(Board, 8),             % Extracts the board from gamestate, write the columns and rows as letters 
    nl.                                 % and numbers, respectively.


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


extract_board(GameState, GameState).

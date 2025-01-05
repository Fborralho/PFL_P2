
:-ensure_loaded('display.pl').
:-ensure_loaded('move.pl').
play_game :-
    init_game_state(GameState),
    game_loop(GameState).

% Recursive game loop

game_loop(GameState) :-
    display_game(GameState),
    \+ check_win_condition(GameState),  % Continue if no winner
    get_currentPlayer(GameState, Player),
    format('~w\'s turn. Enter your move (FromRow, FromCol, ToRow, ToCol):~n', [Player]),
    input_move(GameState, Player, (FromRow, FromCol, ToRow, ToCol)),  % Get valid move
    move(GameState, (FromRow, FromCol, ToRow, ToCol), NewGameState),  % Execute 
    game_loop(NewGameState).  

% Ask Player for a move

input_move(GameState, Player, Move) :-
    get_board(GameState, Board),
    read((FromRow, FromCol, ToRow, ToCol)),
        (valid_moves(Player, Board, Moves),
        member((FromRow, FromCol, ToRow, ToCol), Moves) ->
            Move = (FromRow, FromCol, ToRow, ToCol);  % If valid continue
            write('Invalid move. Please try again.'), nl,
            input_move(GameState, Player, Move)).  % if invalid repeats

% Check win condition
check_win_condition(GameState) :-
    get_capturedPieces(GameState, captured_pieces(BlackCount, WhiteCount)),
        (BlackCount >= 12 ->
            write('Black wins! Congratulations!'), nl, true;
        WhiteCount >= 12 ->
            write('White wins! Congratulations!'), nl, true;
        fail).  % Continue the game if theres no winner





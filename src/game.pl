
:-ensure_loaded('display.pl').
:-ensure_loaded('move.pl').
:-ensure_loaded('menu.pl').
:-use_module(library(random)).

play_game :-
    main_menu.

% Recursive game loop

game_loop(GameState) :-
    display_game(GameState),
    \+ check_win_condition(GameState),  % Continue if no winner
    get_currentPlayer(GameState, Player),
    get_board(GameState,Board),
    format('~w\'s turn. Enter your move (FromRow, FromCol, ToRow, ToCol) or type "stop":~n', [Player]),
    input_move(GameState, Player, Move),  % Get valid move
    (Move = stop -> write('Game stopped. Thanks for playing!'),nl;
     move(GameState, Move, NewGameState),  % Execute 
    game_loop(NewGameState)).



game_loop_vs_computer(GameState, PlayerSide) :-
    display_game(GameState),
    \+ check_win_condition(GameState),  % Continue if no winner
    get_currentPlayer(GameState, CurrentPlayer),
    get_board(GameState,Board),
    (CurrentPlayer = PlayerSide ->
        format('~w\'s turn. Enter your move (FromRow, FromCol, ToRow, ToCol) or type "stop":~n', [PlayerSide]),
        input_move(GameState, PlayerSide, Move),
        (Move = stop ->
            write('Game stopped. Thanks for playing!'), nl;
            move(GameState, Move, NewGameState),
            game_loop_vs_computer(NewGameState, PlayerSide));
        
        format('Computer (~w) is thinking...~n', [CurrentPlayer]),
        get_board(GameState, Board),
        valid_moves(CurrentPlayer, Board, Moves),
        random_member(ComputerMove, Moves),  % Pick a random valid move
        move(GameState, ComputerMove, NewGameState),
        game_loop_vs_computer(NewGameState, PlayerSide)
    ).


% Ask Player for a move

input_move(GameState, Player, Move) :-
    get_board(GameState, Board),
    read(Input),
    (Input = stop ->
        Move = stop;
        (Input = (FromRow, FromCol, ToRow, ToCol),
        valid_moves(Player, Board, Moves),
        member((FromRow, FromCol, ToRow, ToCol), Moves) ->
            Move = (FromRow, FromCol, ToRow, ToCol);  % If valid continue
            write('Invalid move. Please try again.'), nl,
            input_move(GameState, Player, Move))).  % if invalid repeats

% Check win condition
check_win_condition(GameState) :-
    get_capturedPieces(GameState, captured_pieces(BlackCount, WhiteCount)),
        (BlackCount >= 12 ->
            write('Black wins! Congratulations!'), nl, true;
        WhiteCount >= 12 ->
            write('White wins! Congratulations!'), nl, true;
        fail).  % Continue the game if theres no winner


update_current_player(GameState, NextPlayer, NewGameState) :-
    get_board(GameState,Board),
    NewGameState = game_state(Board, currentPlayer(NextPlayer), CapturedPieces).

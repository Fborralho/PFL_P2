:- ensure_loaded('game.pl').  


% Display the main menu
main_menu :-
    write('======================'), nl,
    write('      Welcome to       '), nl,
    write('      StormClouds        '), nl,
    write('======================'), nl,
    write('1. How to Play'), nl,
    write('2. Play Game'), nl,
    write('3. Exit'), nl,
    write('Please enter your choice (1 to 3): '),
    read(Choice),
    menu_choice(Choice).

% Handle user selection
menu_choice(1) :-
    display_how_to_play,
    main_menu.  % Return to the main menu after showing the instructions
menu_choice(2) :-
    choose_game_mode.  % Proceed to game mode selection
menu_choice(3) :-
    write('Thank you for playing! Goodbye.'), nl, !.  % Exit
menu_choice(_) :-
    write('Invalid choice. Please try again.'), nl,
    main_menu.

% Display "How to Play" instructions
display_how_to_play :-
    nl,
    write('=== How to Play ==='), nl,
    write('1. The game is played on an 8x8 board.'), nl,
    write('2. Each player takes turns moving their pieces.'), nl,
    write('3. Moves can be capturing or non-capturing.'), nl,
    write('4. Capturing moves eliminate opponent pieces.'), nl,
    write('5. The game ends when a player has captured 12 pieces.'), nl,
    write('6. Enter moves as (FromRow, FromCol, ToRow, ToCol).'), nl,
    write('7. You can type "stop" during your turn to exit the game.'), nl,
    nl.

% Choose game mode
choose_game_mode :-
    write('Select Game Mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('Please enter your choice (1 to 2): '),
    read(Mode),
    game_mode(Mode).

% Handle game mode choice
game_mode(1) :-
    write('Starting Player vs Player mode...'), nl,
    init_game_state(GameState),  % Call the game logic for player vs player
    game_loop(GameState).
game_mode(2) :-
    choose_computer_side.  % Prompt to select which side the player will play
game_mode(_) :-
    write('Invalid choice. Please try again.'), nl,
    choose_game_mode.

% Choose which side to play against the computer
choose_computer_side :-
    write('Choose your side:'), nl,
    write('1. Play as Black'), nl,
    write('2. Play as White'), nl,
    write('Please enter your choice (1 to 2): '),
    read(Side),
    handle_computer_side(Side).

% Handle side selection
handle_computer_side(1) :-
    write('Starting Player (Black) vs Computer (White) mode...'), nl,
    init_game_state(GameState),
    game_loop_vs_computer(GameState, black).  % Start the game with player as black
handle_computer_side(2) :-
    write('Starting Player (White) vs Computer (Black) mode...'), nl,
    init_game_state(GameState),
    game_loop_vs_computer(GameState, white).  % Start the game with player as white
handle_computer_side(_) :-
    write('Invalid choice. Please try again.'), nl,
    choose_computer_side.

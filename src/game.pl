
:-ensure_loaded('display.pl').


test_display:-
    init_game_state(GameState),
    display_game(GameState).
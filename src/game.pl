
:-ensure_loaded('display.pl').

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

test_display:-
    initial_board(Board),
    display_game(Board).
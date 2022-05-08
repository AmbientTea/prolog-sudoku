:- module(solver, [one_of/2, is_not/2, solve/1, example_board/1, block/2, vall_different/1, print_board/1]).


:- use_module(library(chr)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

:- chr_constraint one_of/2.
:- chr_constraint is_not/2.
:- chr_constraint label/1.

is_not(X, Y) \ is_not(X, Y) <=> true.
is_not(X, Y) <=> ground((X,Y)) | X \= Y.

one_of(X, Xs) <=> ground(X) | member(X, Xs).
one_of(_X, []) <=> fail.
one_of(X, [Y]) <=> X = Y.
one_of(X, Xs), is_not(X, NotX) <=> ground(NotX) |
    delete(Xs, NotX, NewXs),
    one_of(X, NewXs).


label(X), one_of(X, Xs) <=> member(X, Xs).
label([H | T]) <=> label(H), label(T).
label([]) <=> true.
label(X) <=> ground(X) | true.

different(X, Y) :- is_not(X,Y), is_not(Y,X).
vall_different([H | T]) :-
    maplist(solver:different(H), T),
    vall_different(T).
vall_different([]).

solve(Board) :-
    maplist(maplist([Cell]>>(one_of(Cell, [1,2,3,4,5,6,7,8,9]))), Board)
    , maplist(vall_different, Board)

    , transpose(Board, BoardTr)
    , maplist(vall_different, BoardTr)

    , bagof(Block, block(Board, Block), Blocks)
    , maplist(vall_different, Blocks)
    , label(Board)
.

block(Board, Block) :-
    Board = [
        [ A, B, C | T1 ],
        [ D, E, F | T2 ],
        [ G, H, I | T3 ]
        | T
    ],
    ( Block = [A, B, C, D, E, F, G, H, I]
    ; block([T1, T2, T3 | T], Block)
    ).
block([[],[],[] | T], Block) :- block(T, Block).


example_board(
    [
        [_, _, 1, _, _, _, 9, _, _],
        [_, 7, _, _, _, 4, 8, _, _],
        [_, 8, 9, _, _, _, _, _, 5],
        [_, 2, _, 5, 4, 1, _, _, 3],
        [_, _, _, _, _, _, 5, _, _],
        [_, 6, _, _, 8, 3, _, 9, 7],
        [7, _, _, 4, _, 8, 3, 6, _],
        [3, 9, 8, 6, 5, _, 4, 2, _],
        [_, _, _, _, 9, _, _, _, _]
    ]
).

print_board(Board) :-
    format("+-------+-------+-------+~n"),
    foreach(
        nth1(Ind, Board, Row),
        (
            format("| ~w ~w ~w | ~w ~w ~w | ~w ~w ~w |~n", Row),
            (member(Ind, [3, 6]) -> format("+-------+-------+-------+~n") ; true)
        )
    ),
    format("+-------+-------+-------+~n").

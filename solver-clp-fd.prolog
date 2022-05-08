:- module(solver, [solve/1, example_board/1, print_board/1]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).

solve(Board) :-
    term_variables(Board, Cells)
    , Cells ins 1..9

    , transpose(Board, BoardTr)
    , maplist(all_distinct, BoardTr)

    , bagof(Block, block(Board, Block), Blocks)
    , maplist(all_distinct, Blocks)
    , label(Cells)
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

% -*- mode: Prolog -*-

% SWI-Prolog version 9.1.0

% Written by Peter Ludemann (peer.ludemann@gmail.com)
% See README.md

:- module('21', [solve/0]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pure_input)).

:- set_prolog_flag(prefer_rationals, true).

solve :-
    solve2('21.input', Humn),
    writeln(Humn).

% This is for solving the 1st part:
%    21.input0: Sum=152
%    21.input:  Sum=142707821472432
solve1(Path, Sum) :-
    parse_lines(Path, Monkeys), !,
    foldl(monkey_op, Monkeys, [], MonkeyOps),
    compute_monkey(root, MonkeyOps, SumExpr),
    !,
    Sum is SumExpr.

% For solving the 2nd part:
%    21.input0: Humn=301
%    21.input:  Humn=3587647562851
solve2(Path, Humn) :-
    parse_lines(Path, Monkeys), !,
    foldl(monkey_op, Monkeys, [], MonkeyOps0),
    member(root-op(_,Sum1,Sum2), MonkeyOps),
    select(humn-_, MonkeyOps0, MonkeyOps),
    compute_monkey(Sum1, [humn-number(Humn)|MonkeyOps], SumExpr1),
    compute_monkey(Sum2, [humn-number(Humn)|MonkeyOps], SumExpr2),
    !,
    iterate_humn(0, SumExpr1, SumExpr2, Humn).

% Use Newton-Raphson to iterate:
%    x1 = x0 - f(x0) / f'(x0)
% We approximate f'(x0) by computing (f(x0+1) - f(x0)) / 1.
% And we assume that all the numbers are integers.
iterate_humn(Humn0, SumExpr1, SumExpr2, Humn) :-
    compute_from_term(Humn, SumExpr1, SumExpr2, Humn0, F0),
    (   F0 = 0
    ->  Humn = Humn0
    ;   compute_from_term(Humn, SumExpr1, SumExpr2, Humn0 + 1, F1),
        (   F1 = 0
        ->  Humn is Humn0 + 1
        ;   Humn1 is Humn0 - integer(F0 rdiv (F1-F0)),
            (   Humn1 = Humn0
            ->  Humn1_ is Humn1 + 1 % Try to avoid infinite loop
            ;   Humn1_ = Humn1
            ),
            iterate_humn(Humn1_, SumExpr1, SumExpr2, Humn)
        )
    ).

compute_from_term(Humn, SumExpr1, SumExpr2, Humn0, F) :-
    copy_term(t(Humn,SumExpr1-SumExpr2), t(HumnX,ExprX)),
    HumnX = Humn0,
    F is ExprX.

compute_monkey(Name, MonkeyOps, Sum) :-
    member(Name-Op, MonkeyOps),
    compute_op(Op, MonkeyOps, Sum).

compute_op(number(N), _, N).
compute_op(op(Op,N1,N2), MonkeyOps, Sum) :-
    compute_monkey(N1, MonkeyOps, S1),
    compute_monkey(N2, MonkeyOps, S2),
    compute_(Op, S1, S2, Sum).

compute_(+, S1, S2, S1 + S2).
compute_(-, S1, S2, S1 - S2).
compute_(*, S1, S2, S1 * S2).
compute_(/, S1, S2, S1 rdiv S2).

monkey_op(Name:number(N), L0, [Name-number(N)|L0]).
monkey_op(Name:op(Op,Name1,Name2), L0, [Name-op(Op,Name1,Name2)|L0]).

parse_lines(Path, Monkeys) :-
    phrase_from_file(lines(Monkeys), Path),
    !.

% TODO: use sequence(line).
lines([Name:OpOrNumber|Lines]) -->
    name_colon(Name),
    op_or_number(OpOrNumber),
    blanks_to_nl,
    lines(Lines).
lines([]) --> [].

op_or_number(number(N)) -->
    integer(N).
op_or_number(op(Op,Name1,Name2)) -->
    string(Name1C), whites,
    operator(Op), whites,
    nonblanks(Name2C),
    { atom_codes(Name1, Name1C) },
    { atom_codes(Name2, Name2C) }.

name_colon(Name) -->
    whites, string_without(": ", NameC), whites, ":", whites,
    { atom_codes(Name, NameC) }.

operator('+') --> "+".
operator('*') --> "*".
operator('-') --> "-".
operator('/') --> "/".



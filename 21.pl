% -*- mode: Prolog -*-

% SWI-Prolog version 9.1.0

:- module('21', [solve/0]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pure_input)).

solve :-
    solve1('21.input0', Sum),
    writeln(Sum).

% This is for solving the 1st part:
%    21.input0: 152
%    21.input:  142707821472432
solve1(Path, Sum) :-
    parse_lines(Path, Monkeys),
    foldl(monkey_op, Monkeys, [], MonkeyOps),
    compute_monkey(root, MonkeyOps, SumExpr),
    Sum is SumExpr.

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
compute_(/, S1, S2, S1 / S2).

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



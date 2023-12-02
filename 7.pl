% -*- mode: Prolog -*-

% Advent of Code 2022, Day 7

% SWI-Prolog version 9.1.0

% Written by Peter Ludemann (peer.ludemann@gmail.com)
% See README.md

:- module('7', [solve/0, solve/2,
                parse_cmds/2,
                normalize_tree/2,
                dirs_at_most/3,
                pretty_tree/1]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pure_input)).
:- use_module(library(pprint), [print_term/2]). % TODO: remove

solve :-
    solve('7.input', Sum),
    writeln(Sum).

solve(Path, Sum) :-
    parse_cmds(Path, Filesys),
    dirs_at_most(Filesys, 100000, Dirs),
    sumlist(Dirs, Sum).

parse_cmds(Path, Filesys) :-
    phrase_from_file(lines([], [], Files), Path),
    !,
    foldl(filesys, Files, [dir([],'/',[])], Filesys0),
    sum_tree(Filesys0, Filesys1, _),
    normalize_tree(Filesys1, Filesys).

:- meta_predicate foldl_tree(+, 3, -, +).

foldl_tree(file(Dir,Filename,Size), Goal, V0, V) :-
    call(Goal, file(Dir,Filename,Size), V0, V).
foldl_tree(dir(Dir,Dirname,Sum,SubdirFiles), Goal, V0, V) :-
    call(Goal, dir(Dir,Dirname,Sum,SubdirFiles), V0, V1),
    foldl_tree(SubdirFiles, Goal, V1, V).
foldl_tree([], _Goal, V, V).
foldl_tree([F|Fs], Goal, V0, V) :-
    foldl_tree(F, Goal, V0, V1),
    foldl_tree(Fs, Goal, V1, V).

% TODO: write foldl_tree/5 that generalizes sum_tree
sum_tree(file(Dir,Filename,Size), file(Dir,Filename,Size), Size).
sum_tree(dir(Dir,Dirname,SubdirFiles), dir(Dir,Dirname,Sum,SubdirFiles2), Sum) :-
    sum_tree(SubdirFiles, SubdirFiles2, Sum).
sum_tree([], [], 0).
sum_tree([F|Fs], [F2|Fs2], Sum) :-
    sum_tree(F, F2, Sum0),
    sum_tree(Fs, Fs2, Sum1), % TODO: make tail recursive with accumulator
    Sum is Sum0 + Sum1.

dirs_at_most(Filesys, AtMost, DirSizes) :-
    foldl_tree(Filesys, dirs_at_most_(AtMost), [], DirSizes).

dirs_at_most_(_AtMost, file(_Dir,_Filename,_Size), V, V) :- !.
dirs_at_most_(AtMost, dir(_Dir,_Dirname,Sum,_SubdirFiles), V0, V) :-
    (   Sum =< AtMost
    ->  V = [Sum|V0]
    ;   V = V0
    ).

dirs(Filesys, Dirs) :-
    foldl_tree(Filesys, dirs_, [], Dirs).

smallest_dir_at_least(Filesys, AtLeast, Dir) :-
    AtLeast2 is AtLeast + 1,
    foldl_tree(Filesys, smallest_dir_at_least_(AtLeast), AtLeast2-nil, _-Dir),
    Dir \= nil.

smallest_dir_at_least_(_AtLeast, file(_Dir,_Filename,_Size), V, V) :- !.
smallest_dir_at_least_(AtLeast, dir(Dir,DirName,Sum,_SubdirFiles), SoFar-V0, SoFar2-V) :-
    (   Sum >= AtLeast -> BigEnough = yes ; BigEnough = no ),
    (   Sum < SoFar -> Min = yes ; Min = no ),
    writeln([dir(Dir,DirName,Sum),atleast=AtLeast,sofar=SoFar, bigenough=BigEnough, min=Min]),
    (   Sum >= AtLeast,
        Sum < SoFar
    ->  SoFar2 = Sum,
        V = dir(Dir,DirName,Sum)
    ;   SoFar2 = SoFar,
        V = V0
    ).

dirs_(file(_Dir,_Filename,_Size), V, V) :- !.
dirs_(dir(Dir,DirName,Sum,_SubdirFiles), V0, [dir(Dir,DirName,Sum)|V0]).

filesys(file(Dir,FileName,Size), DirFiles0, DirFiles) :-
    filesys_(Dir, [], file(Dir,FileName,Size), DirFiles0, DirFiles).

filesys_([], _, file(Dir,FileName,Size), DirFiles, [file(Dir,FileName,Size)|DirFiles]) :-
    (   member(file(Dir,FileName,Size2), DirFiles)
    ->  assert(Size == Size2)
    ;   true
    ).
filesys_([Dir|Dirs], DirSoFar, FileAndSize, DirFiles0, [dir(DirSoFar,Dir,SubDirFiles2)|DirFiles1]) :-
    (   select(dir(DirSoFar,Dir,SubDirFiles), DirFiles0, DirFiles1)
    ->  true
    ;   SubDirFiles = [],
        DirFiles1 = DirFiles0
    ),
    append(DirSoFar, [Dir], DirSoFar2),
    filesys_(Dirs, DirSoFar2, FileAndSize, SubDirFiles, SubDirFiles2).

% Parsing the file of commands and outputs

%! lines(+CurrDir:list(atom), +Files0:list, Files:list)// is det.

% Files0, Files are an input-output pair, each being a list of the
% form file(Dir,Name,Size), where Dir is a list of atoms that gets to
% the directory (e.g., "/foo/bar/zot.txt") would produce
% file([/,foo,bar],'zot.txt',1234).
% CurrDir is of the same form as the Dir in a file.
% The DCG takes a list of codes that contain the commands ("cd", "ls")
% and outputs (with "\n" (code 10). It is designed to be used with
% phrase_from_file/3, so that syntax_error//1 can be used.
lines(CurrDir, Files0, Files) -->
    "$", white, whites, "cd",
    white, whites, nonblanks(Dir0), blanks_to_nl,
    { atom_codes(Dir, Dir0) },
    { (   Dir = '..'
      ->  append(CurrDir2, [_Last], CurrDir)
      ;   append(CurrDir, [Dir], CurrDir2)
      )
    },
    !,
    lines(CurrDir2, Files0, Files).
lines(CurrDir, Files0, Files) -->
    "$", white, whites, "ls", blanks_to_nl,
    ls(CurrDir, Files0, Files1),
    !,
    lines(CurrDir, Files1, Files).
lines(_CurrDir, Files, Files) --> [].
lines(_CurrDir, Files, Files) -->
    syntax_error("Invalid command").

ls(CurrDir, Files0, Files) -->
    \+ "$",
    (   "dir",
        { Files1 = Files0 }
    ;   integer(Size),
        { Files1 = [file(CurrDir, File,Size)|Files0] }
    ),
    white, whites,
    nonblanks(File0),
    { File0 \= [] },
    { atom_codes(File, File0) },
    blanks_to_nl,
    !,
    ls(CurrDir, Files1, Files).
ls(_CurrDir, Files, Files) --> [].

normalize_tree(file(Name,Size), file(Name,Size)).
normalize_tree(file(Dir,Name,Size), file(Dir,Name,Size)).

normalize_tree(dir(Name,DirFiles), dir(Name,SortedNormalizedDirFiles)) :-
    maplist(normalize_tree, DirFiles, NormalizedDirFiles),
    msort(NormalizedDirFiles, SortedNormalizedDirFiles).
normalize_tree(dir(Dir,Name,DirFiles), dir(Dir,Name,SortedNormalizedDirFiles)) :-
    maplist(normalize_tree, DirFiles, NormalizedDirFiles),
    msort(NormalizedDirFiles, SortedNormalizedDirFiles).
normalize_tree(dir(Dir,Name,Size,DirFiles), dir(Dir,Name,Size,SortedNormalizedDirFiles)) :-
    maplist(normalize_tree, DirFiles, NormalizedDirFiles),
    msort(NormalizedDirFiles, SortedNormalizedDirFiles).

normalize_tree([F|Fs], Sorted) :-
    maplist(normalize_tree, [F|Fs], Normalized),
    msort(Normalized, Sorted).
normalize_tree([], []).

pretty_tree(Filesys) :-
    print_term(Filesys, [indent_width(2), tab_width(0), right_margin(50)]).

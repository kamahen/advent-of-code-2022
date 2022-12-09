% -*- mode: Prolog -*-

% SWI-Prolog version 9.1.0

:- module('7', [solve/2, normalize_tree/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pure_input)).
:- use_module(library(pprint), [print_term/2]). % TODO: remove

solve(Path, Filesys) :-
    phrase_from_file(lines([], [], Files), Path),
    maplist(writeln, Files),
    foldl(filesys, Files, [dir('/',[])], Filesys0),
    normalize_tree(Filesys0, Filesys),
    nl,
    writeln('SOLVED'),
    print_term(Filesys, [indent_width(2), tab_width(0), right_margin(40)]).

filesys(file(Dir,FileName,Size), DirFiles0, DirFiles) :-
    tree_to_dir(Dir, file(FileName,Size), DirFiles0, DirFiles).

tree_to_dir([], file(FileName,Size), DirFiles, [file(FileName,Size)|DirFiles]) :-
    (   member(file(FileName,Size2), DirFiles)
    ->  assert(Size == Size2)
    ;   true
    ).
tree_to_dir([Dir|Dirs], FileAndSize, DirFiles0, [dir(Dir,SubDirFiles2)|DirFiles1]) :-
    (   select(dir(Dir,SubDirFiles), DirFiles0, DirFiles1)
    ->  true
    ;   SubDirFiles = [],
        DirFiles1 = DirFiles0
    ),
    tree_to_dir(Dirs, FileAndSize, SubDirFiles, SubDirFiles2).

lines(CurrDir, Files0, Files) -->
    "$", white, whites, "cd", white, whites, nonblanks(Dir0), blanks_to_nl,
    { atom_codes(Dir, Dir0) },
    !,
    { (   Dir = '..'
      ->  append(CurrDir2, [_Last], CurrDir)
      ;   append(CurrDir, [Dir], CurrDir2)
    ) },
    !,
    lines(CurrDir2, Files0, Files).
lines(CurrDir, Files0, Files) -->
    "$", white, whites, "ls", blanks_to_nl,
    ls(CurrDir, Files0, Files1),
    !,
    lines(CurrDir, Files1, Files).
lines(_CurrDir, Files, Files) --> [].

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

normalize_tree(file(File,Size), file(File,Size)).
normalize_tree(dir(Dir, DirFiles), dir(Dir, SortedNormalizedDirFiles)) :-
    maplist(normalize_tree, DirFiles, NormalizedDirFiles),
    msort(NormalizedDirFiles, SortedNormalizedDirFiles).
normalize_tree([F|Fs], Sorted) :-
    maplist(normalize_tree, [F|Fs], Normalized),
    msort(Normalized, Sorted).
normalize_tree([], []).

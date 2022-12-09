% -*- mode: Prolog -*-

:- module(test_7,
	  [ test_7/0
	  ]).

:- use_module(library(plunit)).
:- use_module('7').

test_7 :-
    run_tests([ '7'
              ]).

:- begin_tests('7').

expected_tree(Tree) :-
    normalize_tree([dir([], '/', 48381165,
                        [ dir(['/'], 'a', 94853,
                              [ dir(['/','a'], 'e', 584,
                                    [ file(['/','a','e'], 'i', 584)
                                    ]),
                                file(['/','a'], 'f', 29116),
                                file(['/','a'], 'g', 2557),
                                file(['/','a'], 'h.lst', 62596)
                              ]),
                          file(['/'], 'b.txt', 14848514),
                          file(['/'], 'c.dat', 8504156),
                          dir(['/'], 'd', 24933642,
                              [ file(['/', 'd'], 'j', 4060174),
                                file(['/', 'd'], 'd.log', 8033020),
                                file(['/', 'd'], 'd.ext', 5626152),
                                file(['/', 'd'], 'k', 7214296)
                              ])
                        ])],
                   Tree).

test(cmds, Tree == ExpectedTree) :-
    expected_tree(ExpectedTree),
    solve("7.input0", Tree).

:- end_tests('7').

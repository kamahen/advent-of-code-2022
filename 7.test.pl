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

expected_tree_new(Tree) :-
    normalize_tree([dir('/',
                        [ dir('a',
                              [ dir(['/','a'], 'e',
                                    [ file(['/','a','e'], 'i', 584)
                                    ]),
                                file(['/','a'], 'f', 29116),
                                file(['/','a'], 'g', 2557),
                                file(['/','a'], 'h.lst', 62596)
                              ]),
                          file(['/'], 'b.txt', 14848514),
                          file(['/'], 'c.dat', 8504156),
                          dir(['/'], 'd',
                              [ file(['/', 'd'], 'j', 4060174),
                                file(['/', 'd'], 'd.log', 8033020),
                                file(['/', 'd'], 'd.ext', 5626152),
                                file(['/', 'd'], 'k', 7214296)
                              ])
                        ])],
                   Tree).
expected_tree(Tree) :-
    normalize_tree([dir('/',
                        [ dir('a',
                              [ dir('e',
                                    [ file('i', 584)
                                    ]),
                                file('f', 29116),
                                file('g', 2557),
                                file('h.lst', 62596)
                              ]),
                          file('b.txt', 14848514),
                         file('c.dat', 8504156),
                          dir('d',
                              [ file('j', 4060174),
                                file('d.log', 8033020),
                                file('d.ext', 5626152),
                                file('k', 7214296)
                             ])
                        ])],
                   Tree).

test(cmds, Tree == ExpectedTree) :-
    expected_tree(ExpectedTree),
    nl,
    writeln('EXPECTED'),
    print_term(ExpectedTree, [indent_width(2), tab_width(0), right_margin(40)]), nl,
    solve("7.input0", Tree).

:- end_tests('7').

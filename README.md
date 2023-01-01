# Advent of Code 2022

See also
https://twitter.com/hakankj/status/1598358594533019648
https://github.com/hakank/hakank/tree/master/advent-of-code-2022

## Advent of Code Day 7: No Space Left On Device

https://adventofcode.com/2022/day/7

First part solved using SWI-Prolog version 9.1.0.

Second part mostly done - but there appears to be an error in the problem text ...
the text says that 30000000 bytes are needed, yet claims that directory `e`
is big enough at 24933642 bytes.

`swipl -g solve -t halt 7.pl`

To run the unit tests:
`swipl -g run_tests -t halt 7.test.pl`

Files:

- `7.pl`: the program
- `7.input0`: the small example input, from the problem statement
- `7.input`: the large input
- `7.dirs0`: the directory from 7.input0, given in the problem
- `7.test.pl`: some unit tests. This also contains `7.dirs0`, as a Prolog tree (see `expected_tree/1`).

See also

https://twitter.com/hakankj/status/1600400189163986944
https://github.com/hakank/hakank/blob/master/advent-of-code-2022/7.pi
https://twitter.com/hakankj/status/1600442193491021824
https://github.com/hakank/hakank/blob/master/advent-of-code-2022/7b.pi

## Advent of Code Day 21: Monkey Math

https://adventofcode.com/2022/day/21

First part was pretty easy; second part required a small change and
used a "logic variable" trick; it also used Newton-Raphson method to
quickly converge on an answer (not guaranteed to work, but it did work
in this situation). For simplicity, uses lists rather than dicts or
RB-trees (there's less than 2000 items). Uses DCGs for more general
input.

`swipl -g solve -t halt 21.pl`

- `21.pl`: the program
- `21.input0`: the small example input, from the problem statement
- `21.input`: the large input

See also https://twitter.com/hakankj/status/1605505871332618240
https://github.com/hakank/hakank/blob/master/advent-of-code-2022/21.pi

This puzzle was also solved with Python 3.12, using the same input
files.  It turned out that the "logic variable" trick wasn't needed,
provided the arithmetic was done with a custom "eval" function (the
`calc()` method); the builtin `eval()` could have been used but it
would have required more work to set up.

`python3 21.py`


# Advent of Code Day 7: No Space Left On Device

https://adventofcode.com/2022/day/7

First part solved using SWI-Prolog version 9.1.0.

Second part mostly done - but there appears to be an error in the problem text ...
the text says that 30000000 bytes are needed, yet claims that directory `e`
is big enough at   24933642 bytes.

`swipl -g solve -t halt 7.pl`

To run the unit tests:
`swipl -g run_tests -t halt 7.test.pl`

Files:

- `7.pl`: the program
- `7.input0`: the small example input, from the problem statement
- `7.input`: the large input
- `7.dirs0`: the directory from 7.input0, given in the problem
- `7.test.pl`: some unit tests. This also contains `7.dirs0`, as a Prolog tree (see `expected_tree/1`).

# Advent of Code Day 21: Monkey Math

https://adventofcode.com/2022/day/21

First part was pretty easy; second part required a small change and
used a "logic variable" trick; it also used Newton-Raphson method to
quickly converge on an answer (not guaranteed to work, but it did work
in this situation).

`swipl -g solve -t halt 21.pl`

- `21.pl`: the program
- `21.input0`: the small example input, from the problem statement
- `21.input`: the large input

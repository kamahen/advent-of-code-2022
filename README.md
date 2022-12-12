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
- `7.dirs0`: the directory from 7.input0, given in the problem
- `7.test.pl`: some unit tests. This also contains `7.dirs0`, as a Prolog tree (see `expected_tree/1`).
- `7.input`: the large input


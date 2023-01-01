# Advent of Code 2022, Day 21

from types import *
import re
from dataclasses import dataclass

# I originally wrote this code using lambda's instead of classes, but
# closures don't behave nicely in Python, so switched to
# classes. Also, for part 2, need a bit more information that's not
# available from a lambda.

# TODO: The iterate_humn() function has a side-effect: it updates
#       monkeys[humn]. It would be better if it restored the initial
#       value.

class Monkey:  # only needed for type declarations
    pass

@dataclass(frozen=True)
class MonkeyWithNumber(Monkey):
    number: int
    __slots__ = ['monkeys', 'number']

    def calc(self, monkeys: dict[str,Monkey]) -> int:
        return self.number

@dataclass(frozen=True)
class MonkeyWithCalc(Monkey):
    op: str
    x: str
    y: str
    __slots__ = ['op', 'x', 'y']

    def calc(self, monkeys: dict[str,Monkey]) -> float:
        match self.op:
            case '+': return monkeys[self.x].calc(monkeys) + monkeys[self.y].calc(monkeys)
            case '-': return monkeys[self.x].calc(monkeys) - monkeys[self.y].calc(monkeys)
            case '*': return monkeys[self.x].calc(monkeys) * monkeys[self.y].calc(monkeys)
            case '/': return monkeys[self.x].calc(monkeys) / monkeys[self.y].calc(monkeys)
            case _:   raise RuntimeError('Unknown op: ' + op)

def solve(path:str) -> None:
    solve1(path)
    solve2(path)

def solve1(path:str) -> None:
    monkeys = {}
    read_input(path, monkeys)
    print('(1) root', int(monkeys['root'].calc(monkeys)))

def solve2(path:str) -> None:
    monkeys = {}
    read_input(path, monkeys)
    monkeys['humn'] = MonkeyWithNumber(0)  # initialize iteration
    iterate_humn(monkeys)
    print('(2) humn', monkeys['humn'].number)

def root_diff(monkeys:dict[str,Monkey]):
    monkey_root = monkeys['root']
    return monkeys[monkey_root.x].calc(monkeys) - monkeys[monkey_root.y].calc(monkeys)

def iterate_humn(monkeys:dict[str,Monkey]) ->  None:
    """
    Use Newton-Raphson to iterate:
      x1 = x0 - f(x0) / f'(x0)
    We approximate f'(x0) by computing (f(x0+1) - f(x0)) / 1:
      x1 = x0 - f(x0) / (f(x0+1) - f(x0))
    And we assume that all the values for monkeys['humn'] are integers.
"""
    while True:
        humn0 = monkeys['humn'].number
        f0 = root_diff(monkeys)
        if f0 == 0:
            return
        monkeys['humn'] = MonkeyWithNumber(humn0 + 1)
        f1 = root_diff(monkeys)
        if f1 == 0:
            return
        humn1 = humn0 - int(f0 / (f1 - f0))
        if humn1 == humn0:
            humn1 += 1 # Try to avoid infinite loop
        monkeys['humn'] = MonkeyWithNumber(humn1)

def read_input(path: str, monkeys: dict[str,Monkey]) -> None:
    with open(path, 'r') as input:
        for line in input:
            r = re.match(r'^\s*([^:\s]+)\s*:\s*(\d+)\s*$', line)
            if r:
                n = int(r.group(2))
                monkeys[r.group(1)] = MonkeyWithNumber(n)
            else:
                r = re.match(r'^\s*([^:\s]+)\s*:\s*([^:\s]+)\s*([*+-/])\s*([^:\s]+)\s*$', line)
                if r:
                    op = r.group(3)
                    x = r.group(2)
                    y = r.group(4)
                    monkeys[r.group(1)] = MonkeyWithCalc(op, x, y)
                else:
                    raise RuntimeError('Invalid line: ' + line)

if __name__ == '__main__':
    solve('21.input')

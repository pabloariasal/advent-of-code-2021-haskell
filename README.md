# advent-of-code-2021-haskell
Haskell Solutions for Advent of Code 2021 - https://adventofcode.com/

# Usage

## Run specific days
```
cabal run ao21 -- 01 02 12
```

## Run all days

```
cabal run ao21
```

# Tests

The minimal examples of each day are modeled as unit tests.
To run all the tests:

```
cabal test
```
or for a specific day:

```
cabal test --test-option 09
```

# To dos
- Add some common utilities, like:
    - strip text
    - convert a binary number to decimal
- Add timings
- Write script to download and print input
- Fix the current Dijkstra implementation (Day 23 only runs with nicuveo's one)

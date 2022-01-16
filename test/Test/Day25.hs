module Test.Day25 (tests) where

import Common
import Day25 (part1)

input =
  "v...>>.vv>\n\
  \.vv>>.vv..\n\
  \>>.>v>...v\n\
  \>>v>>.>.v.\n\
  \v>v.vv.v..\n\
  \>.>>..v...\n\
  \.vv..>.>v.\n\
  \v.v..>>v.v\n\
  \....v..v.>"

expected1 = "58"

tests =
  createTests
    [TestResult "Day 25 - Part 1" expected1 (part1 input)]

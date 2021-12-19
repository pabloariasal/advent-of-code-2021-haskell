module Test.Day14 (tests) where

import Common
import Day14 (part1, part2)

input = "NNCB\n\nCH->B\nHH->N\nCB->H\nNH->C\nHB->C\nHC->B\nHN->C\nNN->C\nBH->H\nNC->B\nNB->B\nBN->B\nBB->N\nBC->B\nCC->N\nCN->C\n"

expected1 = "1588"

expected2 = "2188189693529"

tests =
  createTests
    [ TestResult "Day 14 - Part 1" expected1 (part1 input),
      TestResult "Day 14 - Part 2" expected2 (part2 input)
    ]

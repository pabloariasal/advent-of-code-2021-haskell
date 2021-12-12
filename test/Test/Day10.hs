module Test.Day10 (tests) where

import Common

import Day10 (part1, part2)

input = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
expected1 = "26397"
expected2 = "288957"

tests = createTests [TestResult "Day 10 - Part 1" expected1 (part1 input),
                     TestResult "Day 10 - Part 2" expected2 (part2 input)]

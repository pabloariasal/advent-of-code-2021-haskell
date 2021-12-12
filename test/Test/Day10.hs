module Test.Day10 (tests) where

import Test.HUnit

import Day10 (part1, part2)

input = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
expected1 = "26397"
expected2 = "288957"

test1 :: Test
test1 = TestCase (assertEqual "Example" expected1 (part1 input))

test2 :: Test
test2 = TestCase (assertEqual "Example" expected2 (part2 input))

tests :: Test
tests = TestList [TestLabel "Part 1" test1, TestLabel "Part 2" test2]

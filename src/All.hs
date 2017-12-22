module All where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18

import Utils

test = hspec $ mapM_ (\(name, s) -> describe name s) $(thisModuleName)

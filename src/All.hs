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

import Utils

days = [
  ("Day01", Day01.test),
  ("Day02", Day02.test),
  ("Day03", Day03.test),
  ("Day04", Day04.test),
  ("Day05", Day05.test),
  ("Day06", Day06.test),
  ("Day07", Day07.test),
  ("Day08", Day08.test),
  ("Day09", Day09.test),
  ("Day10", Day10.test),
  ("Day11", Day11.test),
  ("Day12", Day12.test),
  ("Day13", Day13.test),
  ("Day14", Day14.test),
  ("Day15", Day15.test)
  ]

test = hspec $ mapM_ (\(name, s) -> describe name s) days

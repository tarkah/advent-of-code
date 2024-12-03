import gleam/int
import gleam/io
import gleam/list

import aoc/day1

const days = [day1.run]

pub fn main() {
  io.println("AOC 2024 with Gleam!")

  let run = fn(run, i) {
    io.println("")
    io.println("==== DAY " <> int.to_string(i + 1) <> " ====")
    let #(part1, part2) = run()

    { "1: " <> part1 }
    |> io.println

    { "2: " <> part2 }
    |> io.println
  }

  days
  |> list.index_map(run)
}

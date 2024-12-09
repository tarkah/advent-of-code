import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

import env
import time.{Millisecond}

import aoc/day1
import aoc/day2
import aoc/day3
import aoc/day4
import aoc/day5
import aoc/day6
import aoc/day7
import aoc/day8

const days = [
  day1.run, day2.run, day3.run, day4.run, day5.run, day6.run, day7.run, day8.run,
]

pub fn main() {
  case command(env.args()) {
    Error(err) -> io.println_error("ERROR: " <> err <> "\n\n" <> help())
    Ok(Command(help: True, ..)) -> help() |> io.println
    Ok(Command(day: day, ..)) -> run(day)
  }
}

fn run(filter: Option(Int)) {
  io.println("AOC 2024 with Gleam!")

  let run_day = fn(f, num) {
    let now = time.now()

    let #(part1, part2) = f()

    let elapsed = now |> time.elapsed(Millisecond)

    io.println("")
    io.println("==== DAY " <> num |> int.to_string <> " ====")
    io.println({ "1: " <> part1 })
    io.println({ "2: " <> part2 })
    io.println({ "in " <> elapsed |> int.to_string <> "ms" })
  }

  let now = time.now()

  days
  |> list.index_map(fn(f, i) {
    let num = i + 1
    let skip = option.is_some(filter) && filter != Some(num)

    use <- bool.guard(when: skip, return: Nil)

    run_day(f, num)
  })

  let elapsed = now |> time.elapsed(Millisecond)

  io.println("")
  io.println({ "Finished in " <> elapsed |> int.to_string <> "ms" })
}

type Command {
  Command(help: Bool, day: Option(Int))
}

fn command(args: List(String)) -> Result(Command, String) {
  command_loop(args, Command(False, None))
}

fn command_loop(args: List(String), cmd: Command) -> Result(Command, String) {
  case args {
    ["--help", ..rest] -> command_loop(rest, Command(..cmd, help: True))
    [day, ..rest] ->
      int.parse(day)
      |> result.replace_error("day must be an int")
      |> result.then(fn(day) {
        command_loop(rest, Command(..cmd, day: Some(day)))
      })
    [] -> Ok(cmd)
  }
}

fn help() -> String {
  "usage: aoc [options] [day]

args:
  [day]   Run a specific day, otherwise run all days

options:
  --help  Print help"
}

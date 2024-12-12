import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

import env
import time.{Millisecond}

import aoc/day
import aoc/day1
import aoc/day10
import aoc/day11
import aoc/day12
import aoc/day2
import aoc/day3
import aoc/day4
import aoc/day5
import aoc/day6
import aoc/day7
import aoc/day8
import aoc/day9

const days = [
  day1.day, day2.day, day3.day, day4.day, day5.day, day6.day, day7.day, day8.day,
  day9.day, day10.day, day11.day, day12.day,
]

pub fn main() {
  case command(env.args()) {
    Error(err) -> io.println_error("ERROR: " <> err <> "\n\n" <> help())
    Ok(Command(help: True, ..)) -> help() |> io.println
    Ok(Command(day: day, validate: validate, ..)) -> run(day, validate)
  }
}

fn run(filter: Option(Int), validate: Bool) {
  io.println("AOC 2024 with Gleam!")

  let now = time.now()

  days
  |> list.index_map(fn(d, i) {
    let num = i + 1
    let skip = option.is_some(filter) && filter != Some(num)

    use <- bool.guard(when: skip, return: Nil)

    day.run(d, num, validate)
  })

  let elapsed = now |> time.elapsed(Millisecond)

  io.println("")
  io.println({ "Finished in " <> elapsed |> int.to_string <> "ms" })
}

type Command {
  Command(help: Bool, validate: Bool, day: Option(Int))
}

fn command(args: List(String)) -> Result(Command, String) {
  command_loop(args, Command(False, True, None))
}

fn command_loop(args: List(String), cmd: Command) -> Result(Command, String) {
  case args {
    ["--help", ..rest] -> command_loop(rest, Command(..cmd, help: True))
    ["--no-validate", ..rest] ->
      command_loop(rest, Command(..cmd, validate: False))
    ["--" <> option, ..] -> Error("Unknown option --" <> option)
    ["-" <> option, ..] -> Error("Unknown option -" <> option)
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
  --no-validate  Don't validate day output vs expects
  --help         Print help"
}

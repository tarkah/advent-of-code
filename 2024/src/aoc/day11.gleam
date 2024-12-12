import gleam/bool.{guard}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/pair
import gleam/result
import gleam/string

import aoc/day.{Day, Expects}

pub const day = Day(
  example,
  input,
  Expects(#(55_312, 65_601_038_650_482), #(194_557, 231_532_558_973_909)),
  run,
)

fn run(input: String) {
  let stones =
    input
    |> string.split(" ")
    |> list.map(int.parse)
    |> result.values

  // Cache FTW
  let cache = dict.new()

  let #(part1, cache) = stones |> blink_for(25, cache)
  let #(part2, _) = stones |> blink_for(75, cache)

  #(part1, part2)
}

fn blink_for(
  stones: List(Int),
  times: Int,
  cache: Dict(#(Int, Int), Int),
) -> #(Int, Dict(#(Int, Int), Int)) {
  stones
  |> list.fold(#(0, cache), fn(a, stone) {
    let #(acc, cache) = a

    let #(count, cache) = blink(stone, times, cache)

    #(acc + count, cache)
  })
}

fn blink(
  stone: Int,
  times: Int,
  cache: Dict(#(Int, Int), Int),
) -> #(Int, Dict(#(Int, Int), Int)) {
  case times {
    0 -> #(1, cache)
    _ -> {
      case dict.get(cache, #(stone, times)) {
        Ok(count) -> #(count, cache)
        Error(_) -> {
          let #(count, cache) = case stone {
            _ if stone == 0 -> blink(1, times - 1, cache)

            _ -> {
              let split = split_digits(stone)

              case split {
                Error(_) -> {
                  blink(stone * 2024, times - 1, cache)
                }

                Ok(#(lhs, rhs)) -> {
                  let #(lhs_count, cache) = blink(lhs, times - 1, cache)
                  let #(rhs_count, cache) = blink(rhs, times - 1, cache)

                  #(lhs_count + rhs_count, cache)
                }
              }
            }
          }

          #(count, dict.insert(cache, #(stone, times), count))
        }
      }
    }
  }
}

fn split_digits(a: Int) -> Result(#(Int, Int), Nil) {
  let log_a = log(10, a)

  // n + 1 == num_digits, so n == even ~= digits == odd
  use <- guard(when: log_a % 2 == 0, return: Error(Nil))

  let div = pow(10, log_a / 2 + 1)

  Ok(#(a / div, a % div))
}

fn log(base: Int, a: Int) -> Int {
  log_loop(base, a, base, 0) |> pair.second
}

fn log_loop(base: Int, a: Int, b: Int, n: Int) -> #(Int, Int) {
  case a >= b {
    True -> {
      log_loop(base, a, b |> int.multiply(base), n + 1)
    }
    False -> #(b, n)
  }
}

fn pow(a: Int, n: Int) -> Int {
  pow_loop(a, a, n)
}

fn pow_loop(a: Int, b: Int, n: Int) -> Int {
  case n {
    0 -> 1
    1 -> b
    _ -> pow_loop(a, a * b, n - 1)
  }
}

const example = "125 17"

const input = "8793800 1629 65 5 960 0 138983 85629"

import gleam/int
import gleam/io

import time.{Millisecond}

pub type Day {
  Day(
    example: String,
    input: String,
    expects: Expects,
    run: fn(String) -> #(Int, Int),
  )
}

pub type Expects {
  Expects(example: #(Int, Int), input: #(Int, Int))
}

pub fn run(day: Day, num: Int) {
  let timed = fn(input, expects) {
    let now = time.now()

    let #(p1, p2) = day.run(input)
    let #(e1, e2) = expects

    let elapsed = now |> time.elapsed(Millisecond)

    io.println({ "1: " <> p1 |> int.to_string })
    expect(p1, e1)
    io.println({ "2: " <> p2 |> int.to_string })
    expect(p2, e2)
    io.println({ "in " <> elapsed |> int.to_string <> "ms" })
  }

  io.println("")
  io.println("==== DAY " <> num |> int.to_string <> " ====")
  io.println("--- example ---")
  timed(day.example, day.expects.example)
  io.println("---- input ----")
  timed(day.input, day.expects.input)
}

fn expect(a: Int, b: Int) {
  case a == b {
    False -> {
      let msg =
        "expected " <> b |> int.to_string <> " got " <> a |> int.to_string
      panic as msg
    }
    True -> Nil
  }
}

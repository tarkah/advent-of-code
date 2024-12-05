import gleam/pair
import gleam/result

pub fn pair_try_map(a, b) {
  let #(a, b) =
    a
    |> pair.map_first(b)
    |> pair.map_second(b)

  use a <- result.then(a)
  use b <- result.map(b)

  #(a, b)
}

pub fn uncurry(a) {
  fn(b: #(c, d)) { a(b.0, b.1) }
}

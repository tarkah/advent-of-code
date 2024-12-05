import gleam/list
import gleam/string

pub fn args() -> List(String) {
  get_plain_args()
  |> list.map(string.from_utf_codepoints)
}

@external(erlang, "init", "get_plain_arguments")
fn get_plain_args() -> List(List(UtfCodepoint))

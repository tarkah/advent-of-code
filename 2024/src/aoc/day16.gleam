import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleam/string

import aoc/day.{Day, Expects}

pub const day = Day(
  example,
  input,
  Expects(#(11_048, 64), #(143_564, 593)),
  run,
)

fn run(input: String) {
  let maze = parse(input)

  let assert Some(#(score, all_paths)) = shortest_path(maze, AllShortest)

  let unique_tiles =
    all_paths
    |> list.flat_map(list.map(_, pair.first))
    |> set.from_list
    |> set.size

  #(score, unique_tiles)
}

pub type Maze {
  Maze(
    width: Int,
    height: Int,
    start: Pos,
    move_cost: Int,
    rotate_cost: Int,
    tiles: BitArray,
  )
}

pub type Pos {
  Pos(x: Int, y: Int)
}

pub type Direction {
  Up
  Down
  Left
  Right
}

type Rotation {
  Clockwise
  CounterClockwise
}

type Tile {
  Space
  Wall
  End
  OutOfBounds
}

pub type Visited =
  List(#(Pos, Direction))

type Cache =
  Dict(#(Pos, Direction), Int)

pub type RunFor {
  AllShortest
  SingleShortest
}

pub fn shortest_path(
  maze: Maze,
  run_for: RunFor,
) -> Option(#(Int, List(Visited))) {
  shortest_path_loop(
    maze,
    run_for,
    [#(maze.start, Right, 0, [])],
    dict.new(),
    None,
  )
}

fn shortest_path_loop(
  maze: Maze,
  run_for: RunFor,
  moves: List(#(Pos, Direction, Int, Visited)),
  cache: Cache,
  min_path: Option(#(Int, List(Visited))),
) -> Option(#(Int, List(Visited))) {
  case moves {
    [] -> min_path
    [#(pos, facing, score, visited), ..rest] -> {
      case min_path {
        // This already cost more than a known min path
        Some(#(min_score, _)) if score > min_score && run_for == AllShortest ->
          shortest_path_loop(maze, run_for, rest, cache, min_path)

        Some(#(min_score, _))
          if score >= min_score && run_for == SingleShortest
        -> shortest_path_loop(maze, run_for, rest, cache, min_path)

        _ -> {
          case get(maze, pos) {
            Wall | OutOfBounds ->
              shortest_path_loop(maze, run_for, rest, cache, min_path)

            End -> {
              let visited = [#(pos, facing), ..visited]

              let min_path = case min_path {
                // This is the same, add tiles
                Some(#(min_score, all_visited)) if score == min_score ->
                  Some(#(score, [visited, ..all_visited]))

                // This is the new minimum path
                _ -> Some(#(score, [visited]))
              }

              shortest_path_loop(maze, run_for, rest, cache, min_path)
            }

            Space -> {
              case cache |> dict.get(#(pos, facing)) {
                // Already visited this at a lower cost
                Ok(cached_score)
                  if score > cached_score && run_for == AllShortest
                -> {
                  shortest_path_loop(maze, run_for, rest, cache, min_path)
                }

                Ok(cached_score)
                  if score >= cached_score && run_for == SingleShortest
                -> {
                  shortest_path_loop(maze, run_for, rest, cache, min_path)
                }

                _ -> {
                  // Cache score for this position so we can skip this
                  // position if encountered at a higher score
                  let cache = cache |> dict.insert(#(pos, facing), score)

                  // Add to visited
                  let visited = [#(pos, facing), ..visited]

                  // Move each direction
                  let new_moves =
                    [None, Some(Clockwise), Some(CounterClockwise)]
                    |> list.map(fn(rotation) {
                      let #(new_pos, new_facing) = move(pos, facing, rotation)

                      let new_score = case rotation {
                        Some(_) -> score + maze.rotate_cost + maze.move_cost
                        None -> score + maze.move_cost
                      }

                      #(new_pos, new_facing, new_score, visited)
                    })

                  shortest_path_loop(
                    maze,
                    run_for,
                    // BFS much faster than DFS due to
                    // caching impact
                    rest |> list.append(new_moves),
                    cache,
                    min_path,
                  )
                }
              }
            }
          }
        }
      }
    }
  }
}

fn rotate(facing: Direction, rotation: Rotation) -> Direction {
  case facing, rotation {
    Up, Clockwise -> Right
    Down, Clockwise -> Left
    Left, Clockwise -> Up
    Right, Clockwise -> Down
    Up, CounterClockwise -> Left
    Down, CounterClockwise -> Right
    Left, CounterClockwise -> Down
    Right, CounterClockwise -> Up
  }
}

fn move(
  pos: Pos,
  facing: Direction,
  rotation: Option(Rotation),
) -> #(Pos, Direction) {
  let new_direction = case facing, rotation {
    _, None -> facing
    _, Some(rotation) -> rotate(facing, rotation)
  }

  let new_pos = case new_direction {
    Up -> Pos(..pos, y: pos.y - 1)
    Down -> Pos(..pos, y: pos.y + 1)
    Left -> Pos(..pos, x: pos.x - 1)
    Right -> Pos(..pos, x: pos.x + 1)
  }

  #(new_pos, new_direction)
}

fn get(maze: Maze, pos: Pos) -> Tile {
  case pos.x, pos.y {
    x, y if x < 0 || y < 0 || x >= maze.width || y >= maze.width -> OutOfBounds

    _, _ -> {
      let index = pos.y * maze.width + pos.x

      let assert Ok(tile) =
        bit_array.slice(maze.tiles, at: index, take: 1)
        |> result.try(bit_array.to_string)

      case tile {
        "#" -> Wall
        "." | "S" -> Space
        "E" -> End
        _ -> panic as "invalid tile"
      }
    }
  }
}

pub fn parse(input: String) -> Maze {
  let lines = input |> string.split("\n") |> list.map(string.to_graphemes)

  let width = lines |> list.first |> result.unwrap([]) |> list.length
  let height = lines |> list.length

  let #(tiles, start) =
    lines
    |> list.index_fold(#(<<>>, Pos(0, 0)), fn(acc, line, y) {
      line
      |> list.index_fold(acc, fn(acc, char, x) {
        let #(tiles, start) = acc

        case char {
          "S" -> #(bit_array.append(tiles, <<char:utf8>>), Pos(x, y))
          _ -> #(bit_array.append(tiles, <<char:utf8>>), start)
        }
      })
    })

  Maze(width, height, start, 1, 1000, tiles)
}

const example = "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"

const input = "#############################################################################################################################################
#...#.....#.......#...........#...#.............#.......#.........#.....................#.............#.........#.....#.......#.........#..E#
#.#.#.#.###.#.###.#####.#.#.#.#.#.#.###.#######.#######.#.#####.###.#.#######.###.#.#.#.#.#.###.#####.###.#####.#.###.#.###.#.#######.#.#.#.#
#.#...................#.....#...#.#.#.#.......#.........#.....#.....#.........#...#.#.#.#.#.........#.....#...#...#...#.#...#...#.....#...#.#
###.#.#.#.#####.#####.#.#.#######.#.#.#######.#######.#######.###########.#####.#.###.#.#.#########.#######.#.#.###.#####.#####.#.###.#####.#
#...#.#...#...#.#.#...#...#...#...#...#.....#.....#.#.#...#.............#...#...#.#...#...#.......#.....................#.#.....#...#.....#.#
#.###.#####.#.#.#.#.###.#.###.#.#####.#.###.#####.#.#.#.#.#.#####.#.###.#####.#####.#########.#.#.#####.#.#####.#.###.#.#.#.#######.#.###.#.#
#.....#...#.#.#.#...#...#...#.......#.#...#...#...#...#.#...#...#.#...#.......#.....#.........#...#.....#.....#.#.#...#...#.#.......#...#.#.#
#.#####.###.#.#.#.###.#.###.#.#######.###.#.#.#.#.#####.#####.#.#.###.###.#####.#####.#############.#####.###.#.#.###.#####.###.#####.#.#.#.#
#.......#...#...#...#...#...#.#.....#.....#...#.#.#.....#.....#...#...#...#...#.....#...#...........#.#...#.#...#...#...#.....#...#.....#...#
#####.###.#####.###.###.#.#.###.###.#######.###.#.#.#########.#.###.#######.#.#####.###.#.###########.#.###.###.###.#.#.#####.###.#.#.#.###.#
#...#.#...#.......#.......#.#...#...#.....#.#.#.#.#...#...#...#.#...#.....#.#.........#.#.....#.........#.....#.....#.#.....#.....#.#.#.#...#
#.###.#.###.###.###########.#.###.#.#.#.#.#.#.#.#####.#.#.#.###.#.#.#.###.#.#########.#.#####.#.#####.###.###.#######.#####.#######.#.#.#.###
#.....#.#.......#.......................#.#.....................#.#.#...#...#.......#.......#.#.#...#.#...#.#.#.#.........#.#.....#.#.#.#...#
#.#####.#.#####.#.#.#.#######.#.#.#.#.#.#.#.#.###.#######.#####.#.#####.#####.###.#.#######.#.###.#.#.###.#.#.#.#.#########.#.###.###.#.###.#
#.....#.#.....#...#.#.#.........#.#.#.#.....#...#.........#...#.#.........#...#.....#...#...#.#...#.#...#.#.#.#...#.....#...#...#...#.......#
#######.#####.#####.#.#####.###.#.###.###.#.#.#####.#######.#.###########.#.###.#####.###.###.#.###.###.#.#.#.#####.###.#.###.#.###.#.###.###
#.......#...#.#...#...#...#...#.#.....#...#...#...#.......#.#.#.......#...#.#.#.#...#.....#.#...#.#.#.#...#...#.....#...#.#...#.#.#...#...#.#
#.#######.#.#.###.#####.#.#.#.#.#######.#######.#.#.#######.#.#.#####.###.#.#.#.#.#.#.#####.#.#.#.#.#.#####.###.#.###.###.#.###.#.###.#.###.#
#...#...#.#.............#.#...#...#.....#...#...#...#.........#...#.#...#.#.#.....#.#.......#.....#.#.....#.....#...#.#...#...#.#.....#.....#
###.#.#.#.#########.#.###.###.###.###.###.#.#.#.###.#.#######.###.#.###.###.#.###.#.#######.#######.#.###.#######.#.#.#.#######.#.#########.#
#...#.#...#...#.......#.........#...#.#...#.#.....#...#.....#...#.#.........#.....#.............#...#...#.#.....#.#.....#.......#.........#.#
#.#####.###.#.###############.#####.#.#.###.#####.#######.#####.#.#####################.#########.#####.#.#.#####.#####.#.#.###.#######.###.#
#.#...#.#...#.......#.....#.......#...#...#.......#.......#.....#.#...#.....#.........#...........#.....#.#.#.....#.....#.#...#.........#...#
#.#.#.###.#########.#.###.#.###.###.#.###.#.#######.#.#####.###.#.#.#.###.#.#.#####.#.#.#########.#.#.#.#.#.#.#####.#.#####.#.#.#####.###.###
#...#...#.#.....#.#.#...#...#.#.......#.....#.....#.#.....#...#.....#...#.#.#.#...#.#.#.............#...#...#.#...#.#.#...#.#.......#.#...#.#
#.#####.#.#.###.#.#.###.#####.#########.#####.###.###.#.#.###.#.#######.#.###.#.#.#.###.#############.#.#.#.#.#.###.#.#.#.#######.#.###.###.#
#.#...#.....#.....#.....#.....#.......#.....#...#...#...#...#.#.....#...#.......#.#.#...#...#.......#.#.#.#...#.#...#...#.#.....#.#.#...#...#
#.#.#.#.#########.###########.#.###.#######.###.###.#######.#.###.###.#############.#.#####.#.#####.###.#.#.###.#.#######.#.###.#.#.#.###.#.#
#.#.#.#...#...#.#.....#.....#...#.#.#.....#...#.#.#.#.......#...#.#...#.........#...#.......#.#...#...#.#.#.#...#.#.....#.#.#...#.#...#...#.#
#.#.#.#.###.#.#.#####.###.#.#.###.#.#.###.#.#.#.#.#.#.#####.###.#.#.###.#######.#.#######.###.#.#.###.#.#.#.#.#.#.#.###.#.#.#.###.#####.###.#
#.#.#.#.#...#.#.#...#.#...#.#.#...#.....#...#.....#.#.....#.#.#.#.#.#.......#...#.......#.#...#.#...#.#.#.#.#.#...#.#...#...#...#.#.....#...#
###.#.###.###.#.#.#.#.#.###.#.#.#.#########.#######.#.###.#.#.#.###.#######.#.#######.###.#.#####.###.#.###.#.###.#.###.#######.#.#.#######.#
#...#.....#.#.#.#.#...#.#...#.#.#...#.......#.......#...#.....#.....#...#.....#.......#...#.#.....#...#.#...#.....#...#.........#.#...#...#.#
#.#########.#.#.#.#####.###.#.###.#.#.#####.#.#########.#.#########.#.#.#.###.#.#######.###.#.#.###.#.#.#.#########.#.#########.#.###.#.#.#.#
#...#.......#.#.#.#.......#.#...#.#...#.....#...........#.#...#.............#.#.#.......#.#.#.#...#...#.#.#.......#.#...#.......#.....#.#.#.#
###.#.#####.#.#.#.#######.#.###.#.#.#####.###########.#.###.#.#.#####.#####.#.#.#.#######.#.###.#.###.#.#.###.###.#.###.#.#####.#.#####.#.#.#
#...#.....#...#.......#...#...#...#.#...#...#...#.....#.....#.#.#...#.#...#.....#.........#...#.#...#...#.....#.#.#.#.#.#.#.#...#.#.....#.#.#
#.#######.###########.#.#.###.###.#.#.#.###.#.#.#.#.#######.#.#.#.#.#.###.#.#######.#####.###.#.###.###.#####.#.#.#.#.#.#.#.#.#.###.#####.#.#
#.#.......#.........#...#...#...#.#...#...#.#.#...#.#.....#.#.......#...#.#...#.....#...#.#.#.#...#...#.#.......#.#...#...#.#.#.......#...#.#
#.#.#########.###.#.#######.###.#########.#.#.#####.#.#.#.#####.#.###.#.#.#.###.###.#.#.#.#.#.#####.#.#.#.#####.#.#######.#.#.#.#######.###.#
#.#.#.........#...#...#.....#...#.........#.#.#.....#.#.#.....#.#...........#...#.....#.#...#.....#.#.#.#.#...#.#.#.....#...#.#.....#...#.#.#
#.#.###.#########.###.#.#.###.###.###.#####.#.###.###.#.#####.###.#####.#.###.###.#####.#.#######.#.###.#.#.#.#.#.#.###.#####.#.###.#.###.#.#
#.#.....#.#.........#.#.........#.#...#.....#.....#...#.....#...#.#...#.#...#.#...#.....#.#.......#...#.#...#.#.#...#...#...#.......#.#...#.#
#.#######.#.#.#.#.###.#.#######.#.#.###.#####.#.#.#.#######.###.#.#.###.#.#.#.#.###.#######.#######.#.#.#####.#.#####.###.#.###.#####.#.###.#
#.............#.#.#...#.#.....#...#.....#...#.#.#.#.#...#.........#.....#.#...#...#.......#...#.....#.......#.#.#...#.......#.........#.....#
#.#.#.#.#####.#.#.#.###.#.###.#####.#####.#.#.#.#.#.#.###.#.#######.###.#.#.#####.#######.#.#.###########.#.#.#.#.#.###.###.#.#.#.#####.#####
#.#.#.#.#...#.#.#.#.#...#.#...#.....#...#.#.#.#.#.#.#...#.......#.#.....#...#...#...#...#.#.#...........#.....#...#...#...#.#.#.#.....#.#...#
#.#.#.#.#.#.#.#.#.#.#.###.#.###.#####.#.#.#.#.#.#.#.###.#######.#.#####.###.#.#####.###.#.#####.#######.###.#######.#.###.#.#.#.#####.#.#.#.#
#.....#...#...#...#.#...#.#...#...#...#...#...#.#.#...#.......#.....#.....#.#.....#...#.......#.#.....#...#...#.....#.....#.#.#.#.....#...#.#
#.#.#.#.#####.###.#.###.#.###.###.###.###.###.#.#####.#.#.###.#.###.#.###.#.###.#####.#######.#.#.###.###.#.#.#.###.#.#.#.#.#.#.#.#########.#
#.#.#.#.#...#.....#...#.#.#.#.#.......#...#...#.#.....#.#...#.#...#.....#.......#...#.......#.#.#...#.....#.#...#...#...#.#...#...#.........#
#.#.#.###.#.#.###.###.###.#.#.#.#######.###.###.#.#####.#.#.#.###.###.###.#######.#.#######.#.###.#.#.#####.#.#.#.#######.#####.###.#########
#.#.#.#...#...#...#...#...#...#.#...#...#...#.....#.....#...#...#...#.#...#.......#.........#...#.#.#.#...#.#...#.......#...#.....#.#.....#.#
#.###.#.#.###.#.###.###.###.#####.#.###.#.#######.#####.#.###.#####.###.###.###################.###.###.#.#.###########.###.#.#.#.#.#.#.#.#.#
#...#...#...#.#.#...#.....#.......#...#.#.......#.....#.#.#...#...#.....#...#...#...........#.....#.#...#.#...........#.....#.#.#...#.#.#.#.#
###.#####.#.#.#.#.#.#.###.###########.#.#######.#####.###.#####.#.#######.###.#.#.#####.#.#.#.###.#.#.###.###.#######.#######.#.#######.#.#.#
#...#...#.#.#.#...#.#...#.#.......#...#...#.....#...#.....#...#.#.....#...#...#...#...#.#.#...#...#.#...#.#...#.....#.#.....#.#.........#.#.#
#.###.#.#.#.#.###.#.###.#.#.#####.#.#####.#.#######.###.#.#.#.#.#####.#.###.#########.#.#.#####.###.#.###.#####.###.###.###.#.#.#########.#.#
#.#...#.#.#...#.....#.#.#.#.#...#.#.......#.......#.#...#...#.....#...#.#.....#.......#.#.#...#.#.....#...#.....#.#.....#.#...#.............#
#.#.#.#.#.###.#.#####.#.#.#.###.#.#######.#.#####.#.#.#####.#######.###.#####.#.#####.#.#.#.#.#.#######.###.#####.#######.#######.###.#.###.#
#...#.#...#...#.#.......#.#...#.....#...#.#.....#.#.#.#...#.......#...#.....#.#.#...#.#...#.#...#.....#...#.#.#.......#.........#.#...#.....#
#.###.###.#.#.#.#########.###.#.#####.#.#.#######.#.#.#.#.#.#####.#.#.#####.#.#.#.#.#.#.###.#####.###.###.#.#.#.###.###.#.#.###.#.#.###.#####
#.#...#...#.#.............#...#.#.....#...#.......#.#...#...#...#.#.#.#.....#.#.#.#.#.#.....#.#...#.#.....#.#.#.#.#.....#...#.#.#.#.#.#...#.#
#.#.###.#.#.#.###.#.#.#####.#####.#######.#.#######.#########.###.#.#.#.#####.#.#.#.###.#.###.#.#.#.#####.#.#.#.#.###.###.###.#.#.#.#.###.#.#
#.#...#.#...#...#...#.#...#.........#...#...#.#.............#...#...#.#.#.....#...#.....#...#...#.#.....#.#.#.....#...#...#.#...#.#.#.....#.#
#.###.#######.#.#.#.###.#.###########.#.#####.#.#########.###.#.###.#.#.###.###.#####.#####.###.#.#.###.#.#.#######.#.#.###.#.###.#.#.#####.#
#.....#.......#...#.....#.#...........#.#...#...#.....#.#...#.#.....#.#...#...#.#...#.#...#...#.#.#...#...#.#.....#.#.#.#...#.......#.#.....#
#.#.###.#######.#########.#.#####.#####.#.#.#.###.###.#.###.#.#######.###.###.#.#.#.#.###.###.###.###.#####.#.###.#.#.#.#.###.#.#####.#.#####
#.#.....#.....#.#.#.....#...#...#...#...#.#.#.#...#.#...#.#.#.........#...#...#.#.#.#.#.....#.#...#.#.#.....#...#...#.#.#.....#...#...#.....#
#.#.#######.#.#.#.#.###.###.#.#.#####.###.#.#.###.#.###.#.#.#########.#.###.###.#.#.#.#.#####.#.###.#.#.#####.#.#####.#.#####.#.#.#.#######.#
#.#.......#.#...#.#.#.#...#.#.#...#.......#.....#.#.....#.#.....#.....#.#.....#.#.#.#...#.....#.#.....#.#...#.#...#.#.......#.#.#.#.........#
#.#######.#.#####.#.#.###.#.#.###.#.#######.###.#.###.###.#####.#.#######.###.###.#.###.#.#####.#.#####.#.###.###.#.#.#####.###.#.#########.#
#...#.#...#...#...#.#...#.#.#.#...#.#.....#...#.#...#.....#.....#.......#.#...#.......#.#...#...#.#.....#.......#.#.#.......#...#.#...#...#.#
#.#.#.#.#####.#.#.#.#.###.###.#.#.#.#.###.###.#.#.#.#####.#.#.#########.#.#.###.#####.#.###.#.#####.#########.###.#.#.#.###.#.#.#.#.#.#.#.#.#
#.#.#.#...#.....#.#.#...#.....#.#...#...#.....#.#.#...#...#.#.#.........#.....#.#.#...#.#.#.#.......#.......#.#...#.#.#.#...#.#.#.#.#.#.#...#
###.#.###.#######.#.#.#.#######.###.###.#######.#.###.#####.###.#.###.#.#.###.#.#.#.###.#.#.#####.###.#####.###.###.#.#.###.#.#.#.#.###.#####
#...#...#.......#...#.#.......#...#...#.........#...#.....#.....#...#.#.#...#.#...#.#.#...#.....#.#.....#.......#.....#...#.#.#...#...#...#.#
#.#####.#######.#.###.#####.#####.###.#.#####.###.#######.#########.#.#.#####.#.###.#.#.#######.#.###.#.#########.#######.#.#.#.#.###.###.#.#
#.#...#.......#.#...#.#...#.#...#.....#.#.........#.......#...#...#.#.#.....#...#...#...#.....#.#...#.#...#.............#.#.........#.....#.#
#.#.#.#.###.#.#.#.#.#.#.#.#.#.#.###.###.###########.#####.#.#.#.###.#.#####.#####.###.###.###.#.###.#.###.#############.#.###.#.#.#.#.#####.#
#.#.#.#.#...#.#.#.#.#.#.#...#.#.......#.#...........#.....#.#...#...#.#...#.....#.#.#...#.#.#...#.#.#.#...#.......#.......#...#...#.#.#...#.#
#.#.#.#.#.#####.#.#.###.###.#.#######.#.#.#.#########.#####.#####.###.###.#####.#.#.###.#.#.#####.#.###.###.#####.#########.#####.#.#.#.#.#.#
#...#.#.#.......#.#...#...#.#.#.....#.....#.#...#...........#.....#.#.........#...#...#.#.#...#...#.#...#...#.#...#.......#.#.#...#.#...#...#
#.#.#.#.#.#########.#.###.###.#####.#########.#.#.#########.#.#####.#####.###########.#.#.#.#.#.#.#.#.#.#.###.#.###.#####.#.#.#.#.#.#####.#.#
#...#...#.#.......#.#...#.#...#...#.....#.............#.....#...#.#.......#...........#.#.#.#.#.#.#...#...#...#.#...#...#.#.#.......#.......#
#.#.###.#.###.###.#####.#.#.###.#.#.#####.###########.###.#.###.#.#.#######.###.#######.#.#.#.#.#.#####.#.#.###.#.###.#.#.#.###.#.#.#######.#
#.#...#.#...#...#.......#...#...#.#.....#...#...#.....#...#.#.#.......#.....#.#.....#.....#.#.#.#...#.....#.....#.#...#...#...#.#.#.#.....#.#
#.###.#.###.###.#########.###.###.#####.###.#.#.#.#.#.#.###.#.#.#.#####.#####.#####.#.###.#.#.#.###.#.#.#.#.#####.#####.#####.###.#.#.###.###
#...............#.#.....#.#...#...#.......#...#.#.#.#.#.......#.#.#.....#.........#...#.#.#.#.#.........#.#.....#.....#.......#...#.#.#.#...#
#.#####.#.#######.#.###.#.#.#.###.#.#.#.#.#####.#.#.#.#####.#.#.###.#####.###.#.#.#####.#.#.#####.#######.#####.#.###.#########.#.#.#.#.###.#
#.#...#...#.....#...#.#...#.#...#.#.#.#.......#.#.#.#.#...#...#.....#...#...#.#.#.#.......#.........#...#.#.....#...#.......#...#.#.#.#.#...#
#.#.#.#.###.#.###.###.#########.#.#.#.#.#.#####.#.###.#.#.###.#######.#####.#.#.#.#####.#.#.#########.#.#.###.#########.###.#.#.###.#.#.#.#.#
#.#.#.#...#.#.#...#.....#.......#...#...#.#.....#...#.#.#.#.#.#.....#.#.....#.#.......#.#.#.#...#.....#.#...#...#.....#...#.#.#...#...#.#...#
#.#.#.#.#.###.#.#####.#.###.#############.#.#####.#.#.#.#.#.#.#.###.#.#.#.###.#.#####.#.#.###.#.#.#####.###.#.#.#.###.#####.#.###.#####.###.#
#.#.#...#...#.........#.....#.....#.....#.#.#.....#.#...#.#.#...#.....#.#.#...#.......#.#.....#...#...#.....#.#...#.#.....#.#...#.........#.#
#.###.#.###.#.###.#.#########.###.#.#####.#.#.#####.#####.#.#.#.#######.#.#.###.#######.###########.#.#######.#####.#####.#.###.#########.#.#
#...#.#.#...#.#...#...#.........#.#.......#.#.#...#.......#.#.#.#.....#.#.#.#.#.......#...........#.#.#.#.......#.......#.#.#...#.......#.#.#
#.#.#.#.#.###.#####.#.#.#########.#.###.###.###.#.###.###.#.#.#.#.###.#.#.#.#.#######.#.#########.#.#.#.#.#.#####.#.###.#.#.#.###.#####.#.#.#
#.#.#.#.#.#.......#.#.....#.......#.......#.....#.#...#...#.#.#...#...#...#.#...........#.......#.#.#...#.#.......#.#...#.#.#...#.....#.#...#
#.#.###.#.#######.#########.#.#####.#######.#####.#.#.#.###.#.#.###.###.###.#.###########.#.#.#.#.#.#.###.###.#.###.#.###.#.###.#####.#.#####
#.#.#...#.....#.......#.....#...#...#.......#...#...#.#.#.#...#.#.#.....#.....#.#...........#.......#.........#.#...#...#.#.#...#.....#.#...#
#.#.#.#.#####.#######.#.#####.#.#####.#.#.###.#.#####.#.#.#.###.#.#######.###.#.#.###########.#######.#####.#.#.#.#.#.#.#.#.#.###.#######.#.#
#.#.#.......#.....#.....#.....#.....#.#.....#.#...#...#.#...#.#.....#.#...#.....#...#.....#...#.....#.#.....#.#...#.....#...#...#.#.......#.#
#.#.#.###.#.###.#.#######.#########.#.#######.###.#####.#.###.#####.#.#.###########.#####.#.###.###.#.#.###.#.###.#.###.#######.#.#.#######.#
#...#.#.#.#.#...#.#...#...#.#.....#...#...#...#.#...#...#.#...#.....#.#.#...........#.....#...#.#.#...#.#...#.....#...#...#...#.#.#.#.....#.#
#.###.#.#.###.#.#.#.#.#.###.#.###.#####.###.###.###.#.###.#.###.#####.#.#.#########.#.#####.#.#.#.#####.#.#####.#####.###.#.###.#.#.#.###.#.#
#...#.#.#...#.#.#...#...#.....#.#.....#...#.#.....#.#.#.#.#.........#.#.......#...#...#...#.#.#.#...#...#...#...#...#.#.....#...#...#.#...#.#
###.#.#.###.#.#.###########.###.#####.#.#.#.#.#.###.#.#.#.#########.#.#########.#.#.###.#.###.#.#.#.###.###.#.#####.#.#######.###.#####.#.#.#
#...#.#...#...#.....#.....#.........#...#.#.#.#.......#...#.......#...#.........#.#.#...#...#.#.#.#.....#.#...#.....#.......#.#.#.#...#.#.#.#
#.#.#.###.#####.###.###.#.#####.#########.#.###########.###.#####.###.#.#########.###.#####.#.#.#.#####.#.#####.###.#####.#.#.#.#.#.#.#.###.#
#...#.....#...#.#.#.#...#.#.#...#.........#.#.......#.....#.#.....#...#...#...........#.......#...#...#.........#.#.....#...#.#.....#...#...#
#.#.#####.#.#.#.#.#.#.###.#.#.###.#######.#.#.#####.#####.#.#.#####.#####.#.###.#######.#######.#.###.###########.#.###.#.###.###########.#.#
#...#.....#.#.#.#.#...#...#.#.#...#...#.#.#.#.#...#.....#...#.#...#.#.....#.....#...#...#.......#...#.......#.......#...#.....#...#.....#.#.#
###.###.###.#.#.#.#####.###.#.#.###.#.#.#.#.#.#.###.#.#.#####.#.#.#.#.#.#####.#.#.#.#####.#########.#.#####.#.#####.#.#########.#.#.###.#.#.#
#.#...#.#.#.#...#.....#.#.....#.....#...#.#.....#...#.#...#.......#.#.#.#.....#.#.#.......#.#.......#.....#.#.#...#.#.#...#.....#...#.#.#.#.#
#.#.#.#.#.#.#####.#.###.#######.#.#.#.#.#.#####.#.#.#.###.#######.#.#.###.#####.#.#########.#.###.#######.#.#.###.#.#.#.#.###.#######.#.#.#.#
#.....#...#.....#.#.#...#.....#.#.#.#...#.......#.#.#...#...#...#.#.#.#.....#...#.......#...#.#.#.....#...#.#.....#...#.#...#.....#.......#.#
#.#.#####.#####.#.###.###.###.#.#.#.#########.###.#.#######.#.#.#.###.#.#####.#########.#.###.#.#####.#.###.#.#.#######.###.#.###.#########.#
#.....#...#...#.#...#.#.....#.....#.......#...#...#.........#.#.#...#.#.#.#...#.......#.#...#.......#.#...#.#.#.........#.#.#.#.#.....#.....#
#####.###.###.#.###.#.#.###.#####.#####.###.###.#############.#####.#.#.#.#.###.#.#####.#.#.###.#####.#####.#.#####.#.###.#.#.#.#####.###.###
#...#...#.....#.....#.#.#...#...#.....#...#.#...#.....#...........#.#.#...........#...#.#.#.....#.....#.....#.....#.#.#.....#...#...#...#...#
#.#.###.#########.#.#.###.###.###.###.###.#.#.#####.#.#.#########.#.#.###.#######.#.#.#.#.#######.###.#.#.#.###.#.###.#.#####.###.#.#.#.###.#
#.#...#...........#.#.#...#.......#.....#.#.#.....#.#.#...#...#.#.#.#.#...#.....#.#.#...#.......#.#...#.#.#.....#.....#.#...#.#...#.#.#.#.#.#
#.#.###############.#.#.###.#######.###.#.#.#####.#.#####.#.#.#.#.#.#.#.#.#.###.#.#.#####.###.###.#.###.#.#.###.#######.#.#.#.#.###.###.#.#.#
#.#.#.......#.....#...#...#.......#...#...#.......#.....#...#...#.....#.#...#...#.#.......#.#.#...#.#...#.#...#.#.....#.....#.#.............#
#.###.###.###.###.#.#####.#######.#.#.#########.#.#.#.###.###.#.#######.#.#.#.###.#######.#.#.#.#####.###.###.###.#.#######.#.#.#########.#.#
#...#.#.#.......#.#.#...#...#...#...#.......#...#.#.#.......#.#.#.......#.#.#.#...#...#...#...#...#.....#.#...#...#.......#.#.#.....#...#...#
#.#.#.#.#########.#.#.#.###.#.#####.#######.#.###.#.#.#.#.#.#.#.#.#######.#.#.#.###.#.#.#########.#.#####.#.#.#.#####.#####.#.#####.#.#.#.###
#.#.#.....#...#...................#.....#.#.#.#.#.#...#.#.#...#.#...#.....#.#.#.....#...#.......#.....#...#.#.#.#...................#.#.#...#
#.#.#####.#.###.#.###.#######.#.#.#####.#.#.#.#.#.#.#.#.#.#.###.###.#######.#.#############.###.#######.###.#.#.###.#.#.#.#.#####.#####.###.#
#.#.........#...#.#...#.......#.#.....#...#.#...#.#.#...#...#...#...#.......#.........#...#.#...........#...#.#.....#.#.#...#...#.#.....#...#
#.###########.###.#.#.#.###.###.#.#.#####.#.###.#.###.###.###.###.###.#######.#######.#.#.#.#############.#####.#####.#.#####.#.#.#.#.###.###
#...#...#.#...#...#.#.#...#.#...#.#.#.....#.#...#...#...........#.....#.......#...#...#.#.#...#...........#.....#.....#.#.....#...#.#...#.#.#
###.#.#.#.#.###.###.#.###.#.#.###.#.#.#####.#.#####.#.###.#############.#.#.###.#.#.###.#.###.#######.#####.#####.###.#.#.###.#####.###.#.#.#
#.#...#...#.#.#.#...#.#...#...#...#.#.....#.#...#.#.#...#...#.....#.....#.#.#...#.#.....#...#.......#.#.....#...#.#...#.#...#.#...#.#.#.#...#
#.#####.###.#.#.#.###.#.#######.#.###.#####.###.#.#.#####.#.#.#.###.#####.#.#.###.#########.#.#####.#.#.#####.###.#.###.###.#.#.#.#.#.#.###.#
#.....#.#...#...#.#.#...#.....#...#...#.................#.#...........................#...#.#.#.....#.#.#.....#...#.#...#...#.#.#.#.#.#.....#
#.###.#.#.#######.#.#######.#.###.#.###.#########.#.#.#.#.#######.###.#.###.###.#####.#.#.#.#.#.#####.#.#####.#.#####.#######.#.#.#.#.#######
#S..#...#...................#...#.....#...........#...#.........#.....#.............#...#.......#.............#...............#.#...........#
#############################################################################################################################################"

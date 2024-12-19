import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}
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

  let assert Some(#(score, tiles)) = shortest_path(maze)

  let unique_tiles =
    tiles
    |> set.size

  #(score, unique_tiles)
}

type Maze {
  Maze(width: Int, height: Int, start: Pos, tiles: BitArray)
}

type Pos {
  Pos(x: Int, y: Int)
}

type Direction {
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
}

type Visited =
  List(#(Pos, Direction))

type Cache =
  Dict(#(Pos, Direction), Int)

fn shortest_path(maze: Maze) -> Option(#(Int, Set(Pos))) {
  shortest_path_loop(maze, [#(maze.start, Right, 0, [])], dict.new(), None)
}

fn shortest_path_loop(
  maze: Maze,
  moves: List(#(Pos, Direction, Int, Visited)),
  cache: Cache,
  min_path: Option(#(Int, Set(Pos))),
) -> Option(#(Int, Set(Pos))) {
  case moves {
    [] -> min_path
    [#(pos, facing, score, visited), ..rest] -> {
      case min_path {
        // This already cost more than a known min path
        Some(#(min_score, _)) if score > min_score ->
          shortest_path_loop(maze, rest, cache, min_path)

        _ -> {
          case get(maze, pos) {
            Wall -> shortest_path_loop(maze, rest, cache, min_path)

            End -> {
              let tiles =
                [#(pos, facing), ..visited]
                |> list.map(pair.first)
                |> set.from_list

              let min_path = case min_path {
                // This is the same, add tiles
                Some(#(min_score, t)) if score == min_score ->
                  Some(#(score, set.union(t, tiles)))

                // This is the new minimum path
                _ -> Some(#(score, tiles))
              }

              shortest_path_loop(maze, rest, cache, min_path)
            }

            Space -> {
              case cache |> dict.get(#(pos, facing)) {
                // Already visited this at a lower cost
                Ok(cached_score) if cached_score < score -> {
                  shortest_path_loop(maze, rest, cache, min_path)
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
                        Some(_) -> score + 1001
                        None -> score + 1
                      }

                      #(new_pos, new_facing, new_score, visited)
                    })

                  shortest_path_loop(
                    maze,
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

fn parse(input: String) -> Maze {
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

  Maze(width, height, start, tiles)
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
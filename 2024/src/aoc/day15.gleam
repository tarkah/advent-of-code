import gleam/bit_array
import gleam/int
import gleam/list
import gleam/result
import gleam/string

import aoc/day.{Day, Expects}

pub const day = Day(
  example,
  input,
  Expects(#(10_092, 9021), #(1_563_092, 1_582_688)),
  run,
)

fn run(input: String) {
  let with_scale = fn(scale) {
    let #(warehouse, robot, moves) = parse(input, scale)
    exhaust(warehouse, robot, moves)
    |> total_gps_coordinate(0, 0)
  }

  let part1 = with_scale(1)
  let part2 = with_scale(2)

  #(part1, part2)
}

type Warehouse {
  Warehouse(width: Int, height: Int, tiles: BitArray, scale: Int)
}

type Robot =
  Pos

type Pos {
  Pos(x: Int, y: Int)
}

type Direction {
  Up
  Down
  Left
  Right
}

type Tile {
  Empty
  Wall
  Box(List(Pos))
}

fn total_gps_coordinate(warehouse: Warehouse, index: Int, sum: Int) -> Int {
  case warehouse.tiles {
    <<"B":utf8, rest:bits>> -> {
      let y = index / warehouse.width
      let x = index % warehouse.width
      total_gps_coordinate(
        Warehouse(..warehouse, tiles: rest),
        index + 1,
        sum + y * 100 + x,
      )
    }
    <<_, rest:bits>> ->
      total_gps_coordinate(Warehouse(..warehouse, tiles: rest), index + 1, sum)
    _ -> sum
  }
}

fn exhaust(
  warehouse: Warehouse,
  robot: Robot,
  moves: List(Direction),
) -> Warehouse {
  case moves {
    [] -> warehouse
    [dir, ..rest] -> {
      case move_boxes(warehouse, robot, dir, False) {
        Error(_) -> exhaust(warehouse, robot, rest)
        Ok(warehouse) -> exhaust(warehouse, move(robot, dir), rest)
      }
    }
  }
}

fn move_boxes(
  warehouse: Warehouse,
  pos: Pos,
  dir: Direction,
  is_moving: Bool,
) -> Result(Warehouse, Nil) {
  let moved = move(pos, dir)
  case get_tile(warehouse, moved), dir {
    Box(positions), Up | Box(positions), Down -> {
      use warehouse <- result.map(
        positions
        |> list.try_fold(warehouse, fn(warehouse, pos) {
          move_boxes(warehouse, pos, dir, True)
        }),
      )

      shift(warehouse, pos, moved)
    }
    Box(positions), Left -> {
      let assert Ok(left) = positions |> list.first

      use warehouse <- result.map(move_boxes(warehouse, left, dir, True))

      positions
      |> list.window_by_2
      |> list.fold(warehouse, fn(warehouse, a) {
        let #(to, from) = a
        shift(warehouse, from, to)
      })
      |> shift(pos, moved)
    }
    Box(positions), Right -> {
      let assert Ok(right) = positions |> list.last

      use warehouse <- result.map(move_boxes(warehouse, right, dir, True))

      positions
      |> list.window_by_2
      |> list.reverse
      |> list.fold(warehouse, fn(warehouse, a) {
        let #(from, to) = a
        shift(warehouse, from, to)
      })
      |> shift(pos, moved)
    }
    Empty, _ if is_moving -> Ok(shift(warehouse, pos, moved))
    Empty, _ -> Ok(warehouse)
    Wall, _ -> Error(Nil)
  }
}

fn shift(warehouse: Warehouse, from: Pos, to: Pos) -> Warehouse {
  let #(warehouse, from_char) = replace(warehouse, from, ".")
  let #(warehouse, _) = replace(warehouse, to, from_char)
  warehouse
}

fn replace(warehouse: Warehouse, pos: Pos, char: String) -> #(Warehouse, String) {
  let index = index(warehouse, pos)

  let assert Ok(before) = bit_array.slice(warehouse.tiles, at: 0, take: index)

  let assert Ok(after) =
    bit_array.slice(
      warehouse.tiles,
      at: index + 1,
      take: bit_array.byte_size(warehouse.tiles) - index - 1,
    )

  let assert Ok(replaced) =
    bit_array.slice(warehouse.tiles, at: index, take: 1)
    |> result.try(bit_array.to_string)

  #(
    Warehouse(
      ..warehouse,
      tiles: bit_array.concat([before, <<char:utf8>>, after]),
    ),
    replaced,
  )
}

fn move(pos: Pos, dir: Direction) -> Pos {
  case dir {
    Up -> Pos(..pos, y: pos.y - 1)
    Down -> Pos(..pos, y: pos.y + 1)
    Left -> Pos(..pos, x: pos.x - 1)
    Right -> Pos(..pos, x: pos.x + 1)
  }
}

fn get_tile(warehouse: Warehouse, pos: Pos) -> Tile {
  case bit_array.slice(warehouse.tiles, index(warehouse, pos), 1) {
    Ok(<<".":utf8>>) -> Empty
    Ok(<<"#":utf8>>) -> Wall
    Ok(<<"B":utf8>>) if warehouse.scale == 1 -> Box([pos])

    Ok(<<"B":utf8>>) ->
      Box([pos, ..until_box_right(warehouse, Pos(..pos, x: pos.x + 1), [])])

    Ok(<<"O":utf8>>) -> {
      let left = until_box_left(warehouse, Pos(..pos, x: pos.x - 1), [])
      let right = until_box_right(warehouse, Pos(..pos, x: pos.x + 1), [])
      Box(list.flatten([left, [pos], right]))
    }

    Ok(<<"X":utf8>>) ->
      Box(
        until_box_left(warehouse, Pos(..pos, x: pos.x - 1), [])
        |> list.append([pos]),
      )

    _ -> panic as "outside warehouse"
  }
}

fn until_box_left(
  warehouse: Warehouse,
  pos: Pos,
  positions: List(Pos),
) -> List(Pos) {
  case bit_array.slice(warehouse.tiles, index(warehouse, pos), 1) {
    Ok(<<"B":utf8>>) -> [pos, ..positions]
    Ok(<<"O":utf8>>) ->
      until_box_left(warehouse, Pos(..pos, x: pos.x - 1), [pos, ..positions])
    _ -> panic as "outside box"
  }
}

fn until_box_right(
  warehouse: Warehouse,
  pos: Pos,
  positions: List(Pos),
) -> List(Pos) {
  case bit_array.slice(warehouse.tiles, index(warehouse, pos), 1) {
    Ok(<<"X":utf8>>) -> [pos, ..positions] |> list.reverse
    Ok(<<"O":utf8>>) ->
      until_box_right(warehouse, Pos(..pos, x: pos.x + 1), [pos, ..positions])
    _ -> panic as "outside box"
  }
}

fn index(warehouse: Warehouse, pos: Pos) -> Int {
  pos.y * warehouse.width + pos.x
}

fn parse(input: String, scale: Int) -> #(Warehouse, Robot, List(Direction)) {
  let assert Ok(#(top, bottom)) = input |> string.split_once("\n\n")

  let lines = top |> string.split("\n") |> list.map(string.to_graphemes)
  let width =
    lines
    |> list.first
    |> result.unwrap([])
    |> list.length
    |> int.multiply(scale)
  let height = lines |> list.length

  let #(tiles, robot) =
    lines
    |> list.index_fold(#(<<>>, Pos(0, 0)), fn(acc, line, y) {
      line
      |> list.index_fold(acc, fn(acc, char, x) {
        let #(tiles, robot) = acc

        let append = fn(tiles, char, times) {
          list.repeat(char, times)
          |> list.fold(tiles, fn(tiles, char) {
            bit_array.append(tiles, <<char:utf8>>)
          })
        }

        case char {
          "O" if scale == 1 -> #(append(tiles, "B", 1), robot)
          "O" if scale == 2 -> #(append(tiles, "B", 1) |> append("X", 1), robot)
          "O" -> #(
            append(tiles, "B", 1) |> append("O", scale - 2) |> append("X", 1),
            robot,
          )
          "." | "#" -> #(append(tiles, char, scale), robot)
          "@" -> #(append(tiles, ".", scale), Pos(x * scale, y))
          _ -> panic as "not a tile"
        }
      })
    })

  let moves =
    bottom
    |> string.split("\n")
    |> list.flat_map(fn(line) {
      line
      |> string.to_graphemes
      |> list.map(fn(char) {
        case char {
          "^" -> Up
          "v" -> Down
          "<" -> Left
          ">" -> Right
          _ -> panic as "not a move"
        }
      })
    })

  #(Warehouse(width, height, tiles, scale), robot, moves)
}

const example = "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

const input = "##################################################
#..OO..O.O....O...O..O...O.......O...OO##....O.OO#
#O.#..#OOO#..O..OO...O.O.....OO..O#O#.OOO.....#..#
#.....O.......OO.#OO....#O.OO.O..OO.O.O.O..##....#
#.O....O...O#...#...OO..#..O........#O..#..O..O..#
#O....O...O.O..OO..OO..#OO.#OO.O......##..O..O...#
#..O.##..#O...O...#.#.O.O..O.#......O..#.#...O...#
#O.O.........O..O........OO....OO......O.....O#..#
#...O....#..O..O........O......O.#.....O..O#..O..#
#.....#OOOOOOO.O.....O.OOO.#.#.........O#O....O..#
#.........OO#...O.O.#.O..O.....O..#.O..OO.O.#..O.#
#OOO...............#O.OOO..O#....O.....O...#....O#
#....O...O.O.#....O.O.O.OO..#......OO..O.OO.O...O#
#O.O..OO..#...........#O...O.O#...O...O..O..O.O..#
#OOO...O...#O.O.O...OO..#OO.OO#..O...O..OO...O...#
#..#........#...O..O...#.O..OO..#O.OO.#O......O..#
#O.........O.#O.......O.#.O.O....O#...OO...O...OO#
#.#...O.O....O.O....#...#.O...............O..#.OO#
#...O.....O..OOO...O..#O..#.......#OO..O..#...O..#
#OO.O#..O.#..O.#.........OO...O.OO..O....OO.O....#
#.O..O...O.......OOO....OO....O.OOO........O..O..#
#O..O..#..O.O.O..O.O.O..#O.......O..OO..O..O.....#
##O#.....#.O.....O#......O.........OOO.O...#.OOO.#
#O.#OO...O..#.O#.O..O.##OO......#..#OO.......O.#O#
#..O...#..O....OOO..#...@....OO.#.O#..OO......O..#
#.O#.....O...O..O#..O.OOO.OO.#O.O.O..O....O..OO.##
#..#.#.O#....#..O.O.OOO.......OO......O.OOO......#
#.....O....O...#.OO.OOO..#...O..O...O..#..O#OO.O.#
#OO.O.......O#...O.O......OO....#.O...OO.O..O.O..#
##O..O..O##..O..OOOO...O..#O..O.OO.O....O....#.#.#
##..OO#O.##...#O.OOO.......O.....O..O..OO.O...OOO#
#...#..OO.......O.O..OO..........O...OO....OO.O#.#
#..O.O..#OO.....O.........O#O.#..#..O...O.OO..O..#
#..O.#O...O............#O.O.O...O..O.O...#...OOO##
#...#OO.O#.OO.......#.......O..O.##....OO....OOOO#
#.O.....O..O...O.....#..O.....O...OO.O.O.......OO#
#...#O...O.O.#O..O..#..OOO#....O##....#OO.#.#OOOO#
#.O.#OOO......O...O..O.#O.OO..O.......O...O..O..##
#O.....O.#....O.OOO..O...#O#O....OOOO........#...#
#O...#.O...O.OO..O.O.O.............OO.O.#...#.O###
#O....OO.O.OOO#...#..#.....O#O..O..O.O..O#.O...O.#
#.........O#.##.OO..O..O..##..O........O..OOO....#
#.#....O.....O.O......O.O.O...OO.O....O..#O..O...#
#....O##O.O..#....O.OO.O...O..O..OOO..O...#..O...#
#O#....#.......O..O..#..OO.......O.OO.....OO.O...#
#O...O#OOO.O......O......OO..O.#.O.....O.O....OOO#
##....O##...#O.....O.#........OOOOO#..O....#.O.O.#
#O...O.....OO.....#O...O......O..OO.O..OOO.O#.O.O#
#O.O..#.O.O.O.O#.O#....OO.O....O.OO......O#OO...##
##################################################

>><^v><>^v<<vv<>vv><v><<^^>>>>^^><>^<^^vvv>>><><>v^^^^<<^<^><>>>v><><vv^v^<>>>^>><>v>v>^v>>v>>>v^<<^>v^<v<v<v^^>>v<^v>vv^><vvv<<^<><>^<>vv>^>^^^v<^^>v^>v>v<<^>>>^^vvv<>v>><^^<><<>v>>>^<v>vvv^^v>^^v<>^^<^^v^><>>><<<v^>v^>v^><v<^<<>^v^><v<^><^<>>>>v^<><^^>>>^^v^^<v^^>^v^v>>v<><<<vv^<<>^>^v>>v^v^<^<<><^>>^vv>>vvv^<v>v^<vv><^<<<vv>vv>^<^^>v>>^^^<^^^^<<^<<<<<<v>^v>^^v>>>^<>vvvv>>><<><>^v>>vvvvv^<<^<>^>v>>^>>>v<v><>^v^v><<^v<vv<v^<v^v>><^v<^>>^>>v<^v>>v>>v^v^v<<>v>><<><^<v<^<<v<<v>>^vv^vv<>v<v^<^^>^^^vv^<<>^>^^vv>^^<>>v^<v>>>><v<vv<v<^><^v<^vvvv><v>><<><>v<^<^vv<<<>><^<>^<^<>^<vv^>>><v<^^>vv<>v^>^<<^<^>^<^vvv>>v^>>>^<^>vv^<<^^<<>>>v><>v>^>^v>><><^<<>v^>>^<<>^v^<^<><^^^v<>^<v^v^v^<<^^<<<^>v>v<>^^^^^>>v>^v^v>><>^<<>^<v>>v^><v><<<>v^<<^^><>v<<v><vv^^<>v<<v<>>>v>v<<^v<vv><v<v>>^<<>vvv<<^>vv<<vvv>v>^>>^^^^^^^vv^v>>>vvvvv>^>^v<>v>><^v^^v^^v><>vv>><v<^<^>^v>^^^<<^><v>v^<^v<>><<^v^<^><v^v^^><<^v><><^^v<v><v>^><<^<<<^v<^>><>^^vv<^<<^^^^^v<vv^<v<^<<>>>^><^^>^v^v>v^<v^v^v^^>^v>^^><>v^v>vv>^>v^><v<>v<v<
>>v<v^<vv<v<><<>><^<^<^>><^<>>^>^^<^v^^^<v<^^vv^^^>>^>v>>v<<v^<>vv^<<>^v><v^v>v<>v<<^^^^<^v^<><vv^^v>v<<>>^>>^<^>><<v^^^>vvv>><>>^<^>v><v<<^<<v<<>v<^vv^v<>^>^v^><<<><^v>><v><<^v<<^<^<>>>>v><v>>v<>>><v>v>v<vv<>^>v<^>^^>v^^vvv>v>v<vv>>v<<vv^^v>><>><<<v^vv<<v^^v<<><v>^^>^^vv>v><>v<>>>>vvvvv<>v><<<<>vv>vv><^v^<^<v^<><<^^vv><^<vv^v>>^<>v>><>vv^>v<vv^<^><^v>^>v>^v<v<>^v^<>v><v<>>vv>>vv<^>^<>v><<vv^<><^<>vv><<<<v<<^v>>>^<>>^v^><>v^>^<<^>v^^^vv>>^vv^<<v><>vv^^v><vv><v<vv^<^>>v<><^<^v><^<<<^^<>>vv>>^^<^<<v>><>^^^<vv^<v<vvv><^^>^<<<^>v>^^v<<<vvv<^v<<vv>vvv<^<<v>v^><^^><>^<vv<>^<v<>v><<<v>>v^v<>>^><v><<^v>><v<^<vv^<^>>>>v>^>^<><v^v^v<v^<vvv<<<^<vv^>>v^^<^><^v><^^>v>v^>^>v>>^>>>>^<<v><v<<vv<<<^^<><v<^^<>v>>^^>v><><><<><v><v<>v>>^>^<v<^>^^<^><^^^v>v>>^^<<>>>vvv^v>v^v^<>^^<^><^vv^>^^^>>^><^><>>^<^^<<>>>v<<><v><>vv>^>v>^<^>^^>v<>>vv>^<^^^><vvvv^^vvv<v<<^<v^<<v<^>v><^<>v<<^>v><>>>><>>><^><^><^<<v^^^>^<v><v<>v>v<<^<>^vvv<>v>>>>^v>v^>v<^>^vv<<^<v<>v>vv^^<<vv<^^>v^<><<<v>v>vv^^>v<^^v><<>v>^>>^^v>>^>vvv>^
^^<v><<>^vvv<^v^>><^<^<>^<<v^v<vv>>vv^<^^<vv>><vv^v<<>^<^><^v>^>v^<v>>>>vv>vv<^>^<>^>>^><^>^<>^<>^^v>^vv<^^v>^>v>v>v<><<<>>^vv^<>vv^^^^^<<v>^^^^<<v<<v<v>v<<<^v^><<^>vvv>><v^^<<^>vv>^<<>>vv><<>^v>v^<vv>^vv<<^<v>>>>>>>>><>><^><^^^^><><<>^^^<<<v^^<<>^>v<^>^^<<><>^<<<>v^<>^v^<>v<^<<v^vv>v><^<^v<><<<>>>v><<<^<^>>^<^^><v<v^>vv^^><v>vvv^><^>^^>v><<v^>^v>^><vvv^<^<^^<^^>>>v^>^>v<v^<<^><<^<v<<>>^>^v<v>v^^vv><vv^>v<<v>^>v^v>>v>vv<<<<<^><^^<^<^^>><vv>^<<<vv^^v^vv^^^>v>vv^v^^>><v>^v<^v^>>>^>>^>>vv^v^<>^<>^><^>>^>^><<><^v^v<vv<<vv^<v^v^><<>v<^><>v>^v^<^>v><^><v<>>^v^vv>>^v<vvvv>><><>^v^vvv^<v^v<v<>^<v^<<v>v<<<>^<<>>v>v>><<^>><^<<<<<<<<^^>^>^^>><<>>^<v>v^^>v>>>><^^v^<>vv>^^vv<^<>v^v<<^^vv>>v^>>v><v^^^^<^v<<vv>vv<>^>v<v>v<><<<v<^v<^v^^^<<^vvv^>>vvv^>>^><v^^><>v<vv><>v>v^<<v><v<^><<v<^<v>vv<<><<v><<^^v>^><<<v<<v>^v>vv^><<<^>>v><><<<vv^>>^^<<^>^v^<>^v>v^>v><>v>>^v>^<^^>>>vv>>v^<>vv^^v>>v<^<>>^<^^>^>vvvv<>vv>>v<><v>>v<>v><^^^<<v<>v>v>>^v<<^<>><>^^<^<v<^^>><^^v<<<^>^>v^<>^><><<^^>>>>^<v<v^>>>>>><<<>^v<v>
>^>v^^<^<^<<^<vvv^>v^<v^^v>^<v><><^<^>><^v<^^^vv<v>v><^>^><<^^<^^v^>>><<v>vv>><><<<^^>>^^>>^^v<^>v^>><>v^v^>^^<^v><v^>^^<>>>v<<v^v<^>v^<vv^><>><^>v>v<^>>^>>>v^>v<^>^<v>>^<<^<v<vv>^<v<<v>^<v^<^<^v<><v>^vvvv<>>^^^<<<vv<>>v>v><vvv>vv^<^^^^^>v<^^<^>>v^^<>>v^vv^^v^><v^^^^>^^vvv^<^^<>vv^v<>><^^>v^>^v>>>^^>>>^<^v<v>^>vvv^^<><>><^vv>>^><<<<<^>>>^<vv><<v>>>v><><>>><vv>v^<^>>><^v>vv<<^<>v<><>vv><^^<^v^<<^v>^>v^vv<>^v^>v<v<>^<vv^v<^>^<<^<>><^^^<><^vv^v^^v^<vv>>^^>^<<^>>>><>^^<^^<v<<<<v>>>^<>^^>v^^^<v^>>^^v>><v>v^vv<v>^<^<<><v^<^<^^vv^v>><v<<^v<<^^^<<^v<v^>vv^^v>^>^v><^>><<^<v^<vv>v^^v><<^v><^v><>^^>^v><>vv^v><<v^<>^^>v^><><><^v>v^v>vv>^v><><<v<><>^<>vv<^<>^<<v^>vv>^v^^^v<vv^v<<<^<><<<<<v>>^v<v<<<v^v<vv<>^v>^<^<vvv>^>v><v<>v><>vvvv>^^v^^<>>>v>>>^^>vv^v<<^>v^<v><vv<^<>v<<^<<<vv^^>^vv>^<><<>^>>v^v>^<^v^<v<^<<>^<>^^<>^<^><vv<>^<<v<<><^>v><^<><vv<<v^<^>^<>>vvvv>^vv^vvv^^v><<^v<><<<^vv>><<^>^<^>v>v<>>^<^>^^v<^v<<^v>>>><>vv<^<v<^^v<>^<<>v^v><<v<v<>>^>^^<<v>^^<vv>vv<>>^>v>v>>^^v>^^>><v><^><v>>v<><^v<^v<^
<>v<v<^<>v^>><^<^<v<>>v^>v<vvvv<>^^><^>^>>^^>^v<^<v^<v><>v^<vv<^<v<^><v<v^>v^<<>>^^>^v<>^v<vv^<<v<>><vv^<^^vvv<><^vvv^><^<<v^^<<v^^><v><^v>^<^v^>v<>>^vv><<^v<<v<<vv<vv^>>>vv>vv^><v<<<^vv><>^v>^><<^v<<>>><^>><>vv^<vv<><><^<>^<v>v>^>>>>vv>vvv>v<>vvv^v^v<^<^v>><<>><><>vvvv<>vv<vv>>v^>><>^v<vv^v^>><^vvv^^<v^><v<^vvv<><><^^^v^<<v<>v^>^v<<>^>>^v^>^>^v>^<^^<>^<^^><>>v>^^><v^<v>^>v<v<<<<<<^v><vv<<^vvv>^>><>^><><v^v^^<vvv<^^<v^^>v<v<>v>^>><<^vv><<>><<v<><v<>^v^^<>v^v<v>v^>vv^<<>^>vvv^v<v^^><>v<<>><<v^<><^^vv<<<^><<><^>vv>v<<<^><vv^>>v<<><>>v^v^v<><<>^v>>v<v<vvvv<^^>><^><<>v^^><<v^v<>v<<v<^<^^<>v^>>^v^<<<>>^v^>^<^<v<v><^<>^v^<<><>v>^^^<<^<v^v^>vvv<v><^v^^>v^>>v^<<><<^^^<^><v<v^v^>><^^>>^<vv^>>>>^>><^v^v<^<v^v<><vv>v^v><>^<v^><v^v><>>^^>v<<<<>v<>v>v^v<><<>^>><><<^><<><^vvv<><<^^<v<v>^v^^>>v<vv<^<<^><<<<>><^v^>v>^v^<^<<v><vv>^^v<<><^^><<^v<>v<<<v^vv^^^>>v<><><>><>vvvvv<>>>vvv<<<^v>^^^>v>>v>vv<vv^vvv^<>>^<<v<>>>>^<<<^v<<^^^<vv<<v>>^v^<>^<><>v>v<^<<<v>^><v<^>^v^v^<vv><>v^>^<^^<vvv><<^>v<^^<^vv^^<v^v
^<>v>><<v^>^v^<>^^^>^>>vvv<>v<v>^<^vv<v^><vv^><>^v<<vv<>v><^^v^^v^^^^<^^v<<^^vv<<<<>vv<v^^v<^v<^>>v<^^>><vvvv^<v^<v<<^<>^^^<<>>v><^><^<v>v>^^>v><>^^vvv>^<^^v<v^v<<<^<>^<v<<<<v><^>^^<>vv<><^><>v<>v><^vvv>>>^>>v^<vv><v<>>>><v<^<^^<>^v><>^^^>^>v><^<v^<^v<>v>>v>v^<>^v<><>>><v>^<<<<vv>>><v>>v>^^v>><><^<<>><<<^<<^v^v^>^^><^<<<<>^><<<^v<<>v<<v>>^vv<^v><<<v><^^^><v^^<^vv>>vv<v^<<>^<^>vv<><>^v><>v^>v><^^<^v^v<^>vv>v><^>^>vv<v>^<^><^^^<>^>>vv<<vv^<v>>vv>vv<v<v<^<<<<<^<^^<<><^^<vvvv^<v^>><<<v>v<>><>><^vv<v<<v>^^<^^><^^<><^>><^>>^vv<^^v^<^v^v^<v>v>^><><<^>v^<>^>><^vvvv^><<<v<v<>>^vvv^vvv<><><>v><>v<vv>v>>>^<><^v>>^>v<><<v<>><>>>v^v>v><vv>^<v<^^^><v<<v<v>>><^<^^v^^><v><>><>><^<>>^<v^<<<v^>v^^v>>>^v>v^vvvvv>^<^^>><v^v^>><vvvv^^>vvv<^v^<^>v<^v^<>><>v^>>v<^<v<v>^>v><v>>><^v>^><v<v<vv^^<<>><<<v>^v^^v>v<><>v><<v>vv<^^v>>^>>^v^<^><^><v>^vv^v<^^v>v>v><^^^<><vv<<^v^<v<v>v>vv^>v^>>^vv^vv^^>v>>><<>^<vv>^>^>>^>^<>vvv^>^^<^^><>^vv><>v>><vv<^vv<^<<v>v^>^<>^^<^^>>v^<>>>>v^>v^^><>><v><^v^>>vv>>v>^^v^>^<^>>v<>v^>v
>^<<<^>><v<<^^><<v^v>^<>^^<>>^v<<<^vv<>><<^<><>^<^v<v^^<>v^<v<<vv^<><<v>^<<>>><vvv^>^>v><^v>^<v<v^<<<>>><^><v<<<>^^<<v>^<<v<vv><v^vv>>^<<v><>>v<vv^^<^^<><v^>^v^<<<v>v^^vv<v^>>^^^v><<^><v^^>vv^^>v<^>v>v><><vv<<><>^<<>>v^^<>vvv>v<<<^^<>^^<vv>^^>><>v>v>^vv>v><>^^><<^v^^vvv<v^^^v<^^<<<v^<v>vv^<^^v<><^>vv>>vv>^<>>^v^>v<^v<<v^>^vvv<v^>>>v^^>>^^v<v^^<>v<v<>^<<^>>v<<<>>v^<<<<v><<v^^<v^>>^^<^>^<<><^^>v<vv>v>v<^<v>><>v<^<v>>>v<vv<<<>^<>^^v^><^<>vv^^>>>^>v^<<>v><<v<<^>^>^<>vvv<vv>><<><>^vv<v><^<<vv>>v>>^<<v^>^^<v>>^>^>v^>><>>v<v<^<<<<><v<>^v^^<^v^^v>v>^v<><>>>^^><<<>v^^^<^v<v><^^v<>v<v><v>^^<<>^^v<>>^vvv>><<<<^vv^v>^v<>^>vv^^vvv>vv^><^<<>>v>^vv^v^>><<v^^>^<>>>vvv>^vv^^<>^vv^>^<><^<v^<<><v<>>>>v><>v>^vv>>vv<^^<^^>^>v><^><v^^<>vv>>><>>^^>>^v^v<^<>^vv^>v<><<^^v<><<v^^<<v><>v^v<>^<>v^^<<^^^^^><>v^v><^^v^<^><vvv<^>^<<^^v><^^>v><v>>^<vvvv><^<v>><<<>v>>>>^<>^<^v><>^<v<v<<>^<<^<^v^>v<<>^>^<v<^v^<>^v<>vv>v^<vv<^v>vv>v<<<>^v<>^><v><<<>^^<^^<v^><><<><^^>vv^^<^<^v>v>vv^>^>><^<v<><^><<>>>>^vv>^<v<><<<v<v>>v^^
vv<v>>>^<<vvv^>>v^^v<v^<><^^v<v>^v^<>><>>vv^>>v<<>v^^<<<>vv>^v^>v<>>^>v<^v><<>>^<>^^<<v>^^>>^v><>^^<>vv>><v>vv<><^^v^^<v^>>>^v^^<v><<<>^^vv><>v<><>>^^<v^v^<vv<><vvv><<vv^>^<><><<v^^><^v>v<^^^>>^v^>^><vv<<<>v>^v><v>^>v<v>^vvv<>v>><vv^v><<vv<^^>>v<>^<>v^v^^>^^<vv^><^<^v>v<vvv^><v^<^<v<><>v^^v>v>vvvv^vv^^^>v>vv<>>^<v>^vv^>v^<<v^v><>>v^>vv>vv^>^v^>>^<><^><v>v^^<v<>^><<>v<^>^^<v><>>><vv><^><v<^><<>^<vv^vv>^>^<^^<<^>v^v>^<^<v<v<^<vv^>>vv^^<<^^vv>>><^>vv^><^<><v^<^^v<><^<v<^v^^^v>>vvv>>^^^^><^>>>vv<>>v>>^^>^^>^v^vv^>>^>>v^v^^^v>>><>^<>>><^<v<<<><<>>^^vv<vvvvv^vv><^v^<^<<v^^<>><<>^<^<<<^^>v>v<<<<^v^v<>v^<<>>v<vvv>^vv><>^^^<>v<vv><v<<><vv<^v<>>^v<<<>v^^^^^>v^^<><<<vv^<>v<^v>v<vvv^<^^>^<<<vv<>^>^<v^<><^><^<^<^v><v^^v>^v^><^^<<^><^^>^>>^v>^<>v>>vv>v<<<^^v<^v<v>^>><v><<^^<<^v><<v<^^^^v>>^v>>>v>^^^^v>^<<<><<v<>v^>v^^>^v>v<vvv>^<>v^^v>^>^v><v<^<><^<^^v>>^<<vv<vv>>v^<vv<v><<^>>><<^>^>v<<^><^v^v^>^>^vv^vv>>vv<<<<v><^v><^>>^>>>v>>^>^>><<>><>v^><^^v<<^^v<>^><^v<<<vv><^^^<>^^v^<>>v^<>^^^^<v>>v><<<><>^v>v
v<v>v<>^>v^<^^<>^^>vvv<>><>>v><><<<>><^^>^<<<v^<vvvvvv<^v>v^>^>^>vv>>^v^v^<><><^><vv<>><<v>^v^vv><<>><vv><>v^<^<>v^<<v>^vv><<v>>vv><^>^>vv^vv<<^v<<v<><vvv<^>vvv><>>v^<^^^v^^<^>^^^>vv>^^<<v<^^>vv^v^<>^<<vvv>v<<>v^vv<v>>v<v<vvv<>>^>>v>vv>^v^^<<v>^vv<>v^>>><^<<>^^v^vv^v><vvv<<>>^^v<^<^<>vv><^>>^<<>v<^v<>^<>v^^vv<>><><^^<^v^<v>>^^<<^^v><^^v^^vvv>v<><^>><^<^<>v>>vv^>>^vv<>>vvv^><v<><>^v<><<>><>^^<<>>v<<^<><<^^v<<^^>><<^<vv<<<v^^<^<>>vv^^>><>>v><^<v^^v<>^>>^v<v^><v><^><<^<v<<vvv<<^>^^><>^<<><<><<^>>vv>>^vv<>^^>>v<>><vvv<<^^v^^<>>vv<><<vv><^^v^><v>^<>^^><^v^>v^v><v<^v^<>^<<>>vv<v<^^vv<v^v^vv<><vvv><vvv^^<vv><<<^v><^vvv<>>^v<>^^<^^<vvv^<v^<<^^>v^<^>v<>>^vv<<>^><>>v^^v>v^<v^^^<>^v^v>>><>^>v<>^^<><^><<<<v<<<vv^>>>v^vv^><>>>v<<<>v<<<v^^>v<^>^v^v<^vv<<<^>v>v^>^^><^>v>v><v><^^v>^vv>>vv^>^<^v>><>vv<vv^<^<v>^v^v>^<>>v^<^^^><v^v^><<<^^>>>^^>><v>^>^^><^^v>v<>vvv><^vv^^^^v^^<^><>>^>>^>^<v^><>^^<v>><vv><vv^>>>^<<v^v<<>>vv^>^>v>v^><v^^^^^^<^>vv<>>^>^>>^>><v>v><><vvvvv<^v^>v^>>v<v<v<^<vv^>>>>^<>^<<v<><^^><
><<>^>>^<^^v^^><<v>v>v^>v>>^^<><^^<^vvv<><<^^<^^><vv<^^v<>^^v^^>^vv><v^^^<>^<<<v><>^<<>vvv^v<v>^^<><>>><<>><^<>>^>^<^v>v>>v^>^^v^^^>^vv><v<<v>vv^v<>v^^^v<v<v>><^<>><>>vv<^>v^>^v<>^^^vv<<>v<<v>^^v><>^<<^>^>>^<<>^^^<<^vv^<<<^^^^<v^^^>>^>v<^<<^^>>^vv^><vv<^^^^<v><v<^v<<vv>v>v>^>>^^^v<<vv><^vv^^>>>^>^^^^<>>>>><^>>^^<><v>^<<v^>v^^vv<v^>^><>>><>>^v<^><>v^>^>>^>^^^>vv>v>^^^^vv<>>>v<<^^>>>v^v<^<^v<v><<>>^^^<>^v>>^<><v<>^><^>>^>>v><><vv^^vvv^<<^^v<vv^<<>>>v>><^<<<><v^>v>><>^>>>v^vv<>vvvv>v>>vvv>>vv^><>^^>v^>v<v<v>>^>v^vvv^^>>v^>><vv><v^<v<>>^>v<v<v>>^<><v<<v<<^<v<><<<>^^>v^v>>vv>^^<vv^v^<>>^<>>^v<^<<><<^><v><>^^vv>^<^^v>>^><^<^^><^v<v>>>^vv>><v>^v>^<v^>><v>^><^vv><v>^^>>v^>^^<^v^<<<v<^<<^v>^<<v>vv<v<<>vv^vv>^>><>v^><<vv>><><v^<^vv^<<>v<<>vv^^^<^vv>v^v><>^>>^v>><^<^^v>v^>vv^^>>^<><><^v<><<^><<><><vvv<<v<<vv^<<<<^<^v<v<v<<v<^>>^v>v<<<v^vvvvv<<^><vvv^<<>><v<><<^<>><^<^<>>^<v>v>><<^><>v>^>^v><>>^>>><<v>v<vvv>>>>>><<<v><>v>>v^^vv<^^<<<>vv^>vv<^^>^<><vv<^^<><v<^>v<v<^>^^<><^^>v^^>^>>v>vvv<<<v^>>>^<^v
v<>^^^>^<^>^<<<<v^>^vv^>><>v<<>>^<v<^v>v^^^>><>>v^^<<><^^^^>>^<^^<v<>v^v>v<<v>^>>v>^>^v<>^>v<<>>^<>^^<^<>>>^v>^<vv>>^><^^>><<^^><>^><<>^>^<v<><^^<vv>>v^v>v>v><^>>>>v<>^<vvv^^>^v<^^v>><<v<<>>>v<<v^>>^<>v<v>v<^>>^>><>>v>^v<>^^<^><>><>v<v><vv<>>^>vv^>^^<v>>v>>v>>^v<v<^^><<<<<^vv><<v>vv^>>v><>><vv^<^v^v^v>><v<^^>vv<>>^^<^<v<^vv^^<<<^<v<^>v^v<vv>v<<><>^v<^<<<vv^<>vv^v<>v>v<>^^^>^^>>v>vvvv><vv^v^v^v><<>v>v>>^<vv<<<v><^>><^v<^v><v>^>v<<<<vv^<^v><<>v^<<<^^^^<>vv>vvv^>^v>v>v><><<<^^vv^^^><v><v>><v^v^^<>v^^>vv>v<>>v><^v>><<><>v<^<>^^vv^^><>v^^^<vv>^>^^^<<>^<v><<<>^<^<<v^vv<<<vv^>>v<>>vvvvv<v>v><^vv>>v>><<><><^^>^>^>^<v><<<vvvvv>^<v<v^v>^^v><<<^v<>>^>>^v>^^v^^^^v>^>>v>vvvv<vv<^v<vv><^>v>^>^<>vv^^>><^^>>v^v<^>vv^<vv^>v^>v>>>>v>v^v^>>v><>v^v^<^>v<^^^^^v<<^>v>>^<v<v<>v>v^v>v^>vvv>vvv^vv^^v>^>v^v<v>^^>v<^vvv><>^v>^vvv>v<><vv<^><^^>v^^>><^<v^>^<^>vv>><<^^<>^<>>^><^^v^<<>>^^v<v^>>v^>v>^v^>v>>>>^>>>^>^><>^v^v^>vv^^<^^^>>^vv^^^>v^>>^<<v^^^v^vv><<^^<<v<v>^>^v^^^>><<v^<>><v>><>^<><<v<v<<>><<^>vv>vv<v<v<<<^
v>vv<<v<^v<v<^v>^v<>>>^>>><^^^><v^<><<<>^v>>^><<v>>^v>>^<>>v^>^^vv<<^^><<>v<vv^v^^^>^<>>^<<<<^^^><^<><^vv><>>v<^^>v>^v^^v><v>^v^>>^<^>vvv>^v<>v<>v>^^^><>^vv^<^<^>v<v<v<v<>^v^<vv<<v<^vv<v<v^^<vvv<<^^v<><>>><^v<v^v<^<<vv>^<>^^^<^><<v>>>v^v<^>v<^>^<vvv<^^^vv<^v<<<<v><<^<vv<v<>>^^^^<<<><v<<><><^>v>><v<>><^<v<^^>^>v^><v^<^<^<<>v^vvv<<^^^><^><^>>><<<<^<^^v<^<^v^<^><<^>>v<^>v<>><^v><v<<v><<<>>vv^<>^^^><^<vv^<^<^<v<<^><>^^><v^vv><^<^>v>>>^<><v<v>><>v<>vv<^^<^<^^<><v^<>v<><<vvv^vv>v^vvvvv<>v>>v><^^<<v>><<>v^>^v^><^<>>vv<>^^<<^<<v<<<v<^^^^><<>v^^<^>^v><><^>>v<>^vv^v>^v>>vv<^<^>vv<^<^<^^^<>v^vv><^^>v><<<<>>v^vv<^<v><>v><v<<<vvv<^<<v^><<<><><^vv<>v<vv<<<<^^v^v<<v><^v^<><<>^<^^vvvv<<>^^vv<>>><v<^<v^^vv^v^<v<><v>><>^v^^^>v>>^>v^>^<<>v>>^v^^<<^^v><>^v^>vv>>v<v<^vv>>^<^v<>>vvvv>>>^^<v<><vvv^<><>^><>>v>><<^<>^>^>v>>>^^<^><^v>><^><><<v<^v<v>>^v^^>v^v>>^v^v<<<^^^<^<^<<^v>^<><vv^vv><>>>v>^<<><>v<v>v>>>^>v<><>vvv^v>^v^vvvv>^<v^^^^>v><v<v>>^<^>v>^>>><<v>>vv<>^^<<>^v^v<<>vv<>^v>v<>^^<<>><<^vv^>><v<v><vv<<>v^
^^><v^^^<v>vv><v>^<<v<^>>vvv<^<vv<>^v>v>v>v<v^vvv<>>v>^v><vv>>^^><>v>v><<v<<vv><v^^><vv>><^>><^v>v^vv^^<<><^>v<vv<^><v>v^>>^><<v^<^v<<>v>^><<<^<<>^vv<>>v<<<<><>v>^^<v>><^><<vv>>>v^>v<^vv>v>^^<^>>v>^^<v<<<<>><v<^<v>>v^vvv^v>v^<^v<v<^^^<^v>>><<>>>v<<<><>^<>^>^>vvv^^^>>v<^<v<v^^<^<^><^^^<^>v^>^<^v><><^vv^^><><^>^<<^>>v^>^v^<<^>v^>^<^>v>vvv<<v^<><<>v>>>>^>>v^><^vv><v^v><<vvv^^>>><<>vvv<^<<><vv>>>^<^v^^>^>v^<^><><<vv^vv<vv^>v^vvv^^^<>^v>^>><^<<>>>>>>v<^>vvv>v^^^<<^>v<^><>>v>vvv<^^><^<^vv>^v<><v<v><>><vv>^<vv>>>^>^<^><^>vv<v>><v^v><><^><<<<v^<vv>>>^>^v><>>v^v>^v<^^^><vv^<><><^^>^^>v>^^^<^v><<<>>v<>>^>^^v>^^><>v<^^<<v><>>v<v<><v><v^^><^^>>><<^>>^^vv><<>><>^<<vvv^>vv<<v<v^<^<vv^<><>>v^<vv<^^<v^^v>^v^v^^<><<<>v<>>^>>^<^<<<<vvv>v<vv^v^>^>>^<^<<>vvv<><v><>vvv>>>>^^>^v<v>v>><v^<^^v<v>^^v>>v<>^^^><^^^><>>^<v^<v^>^^<<>>>v<^v^>^^<vvv<^^^<v^<>^vvv><^<><>>>^v<><><v>^>>vv>^^vv^^<^<>vv<^<^<vv><<>^>><<>><vv^>>^v><^v^^v^<^>v<<><v<^<>>^v^^<^^>^>><>>^><>^><v<>v^>>^^^^^^<^>^^><vv^>vv>>>>>^>v>^><v>^>>^v>>^><<>
>v><>^v^^>><<<<^^><>^><v^vv^v>v<<<>vv^^^v^^<<<>v<<^^<>v<><^v<<^^v<>^v>^v<^v^<vvv^><<v><<>>^<vvvvv<^>v^<><<<>>v><>v><vv>^v<v<<>^<v<v^<^>^vv^^><><v>v>v^>v>><v^^^<<vv><v^<^<^^^<><^>^<><>vvv>v^^<>vvv^^v>v><^>^>^<<<v>^v<^><^<v>vvv<vv<^><^>^>v>>^>^^vvv<>v<<<^>vv^>v>v^v<v>>v><<>vvvv>^>^v<^v<>>v<vvv>v><>v^>>^<^^>v<^^>>>><vvv<^><><v><^v>>^>^^>^^^>^>><^<vv<>^<><vv>^^>v^<<^<^<v<v^v<v<<v^v^^v<<v^^<<v^><vv>^<>vvv><>>v^^>^<^v<^^<vv^<^^<>^^v>>>^>^^v<<<<>>vv>vvv<^>vvvv<^<vv^vv^v^^<>^^>^>v<>>>^<><^>><^>v<<<^><>>^<v<v>>v><<<<^>v^^<v>>^^vv<^<<v^vv<^<><v<>>>v<v<>>^>>^<^<<^v>^>>><^^vv<<vvv<><v<<v<v>>^>v^><^>^vv^<>><<^>^>><v>^>^>v><<>^<<v>v>>^>v^>^^v<>v>^v>v<>>^^>>>>>^><>>^>><<v^<^^>vv<>v>^>>^<v<v<<<<^<^>^<vvv<><<vv^vv<v>>>>^>v<v<v^^>>^>>>>vv><<^v<^<<>^<^>vv>^v<v^^vv>>^^^>>><<<>v^^^<^<^><v><^<^>v<><>><><^v^^>>^<<v><^^><v>>>^<><^><<^v^<>>^v>^^<>^^^^^^v<^<^v<<<<<>>>>vv><v^^v^<^<<<>>^<^v<^>>^>><v^<<>^v<^^v>v^v><v^vv^^<<<^v><^<^v><<<>v>v<>^>^>v>v>^vv<<>^v<<vv><^<vv<>>>^<<<v^^v<^v^><<<<^v^v<v<><><>^^v^<><^v^^<><
^>v^<^><<<vv<vv>v<>>v<<vv>v>v<<<<<<^vv^<<<v^^v<v>>v^vv<vv^<<v<^v>v<^vv^<>^>^v><^>>^vv<<<^^^v>^<^><>vv<<v^>v<v^<<^><^<^>>^<^v<>^^<^v<^><v>><^v>^^<vv<v>v<>>><v>v^<>>v<<<^>^>v<<>>>^<v<>>><><^v<>vvvv^<v^<>v><v^>^^>^^^>^v^v<>^v>^^v<^>vvv<>><^^^<^^<>v>^>>v^<>^^vv<^vv<>v<><^<>^^><<<^^>v>v^^>>v>vv^>vvv>v<<^<v>><<<<>v<vv<><>^<v^>^>v^><>^>vv<><<^^><v^v>vv<v<^<v<<^>^<<<><^>^<>>^<^vv<>v>^<v<<<>vv<^>v<><^v><<<vv><<<v><^v<v<<vv<^v^<v^^<>v><^<vv^>><><^<v<v^<<>>^v>v>^v^<v<<v><v<>><><>>>^>^>>>v><>>^^<<<<<v^vvv<^v^<>^vv<>>v^>^<<vv^>>^^^<>v^v^vv><^vv>><^<>^>v>^<vv><v^^^v^<^^^<<^>^<>>>>^<^v<v<><>>^^<v<>^^v^v<>^>v<^^><^<<v><^>v>^vv<^vv<>>>>><v<^^vv<v<^>vv<^v^v^^v<v<<vv>^><<<<^>vv^><v<<v^>>^>v<v><^<^>^^>v><<^<>v<<v>v<><v^<<^>v>^<><v^<<v<v^v>^>vv^^<<^^v><>>vvv<>>^vv>><<^^<vv<>>>^<^^^><>^vv<v<^^v^^^^^<<v^<<v<>v>^^^v<^<^>v><v^v^v>><<>v><vv>>^vvvv<^^<<<v><<<vvv^>><v>v<^^^><vvv^^<<^vv>v^v^>v^^<>^v<v^>v^v><<<^v>^v>^<v^<v<v<^>^<^v>>v^><v^^^<>^>v>v>^^vvv>v^^>>^v<>>^v^<v>^^>^^<^><><v<><>^v<v<><^<v>>^>>><v><^v<>vvv^>
<>><>>v>vv^<^^<^vvv^v><^><<>vvvv^><<<>v<<<>^><^v><vv><^<v<v><v<^v<vv^^<<^v>>v^>^>vv><vv^vv^<v^vv^^>^<<>v>>>>^^vv<v^^v<>>v>^vv^>^v>v><vv<^v<v<<^v>>v>><v<<v>^^^><>v>>^v>>v^><^vv^^vv<v<<^^v<>^<>v<>><^^v^>v^v>v>vv<<<v^^^^<^^v>vvvvvv^v^^^><<>vv>^v^><>^^v>>><^>v>>><vv<><v^>>v^vv>^<vvvv^vv>v<v>^><<>^v<>vv>vvv^v^vv^v^^<<vv<<^v>>^<>v^v<>>^<v>><^>^<>^^v^v^v>>vv><vv<>v>v>v^>>>>>^><<>^v<>^<^^^v><>v>^^^<>v^>^vv>>>>>><>v>><>>>v>><<^>v^^>v<vv<^^^>v><><<>v^>vv^^^v<<^>v^<>v<>>v>^^^v<v<v^<^>>v<^vvv<v^<>^v<<><v<v<<^v<vv^^vvvv^>v><<vv^<<^<<>v>^^^<^^^><>>^vv>^<<<vv^<v<v<<><^^><v^<<^v^^>^<^v<^^vv><^^>^<^><>^<v<<^^>v>><<^>vv<^>^><<v<<v^<<^v<^^^vv>>^>^>v><>^>>^<vv>>v>^^>vv<<<>>><>^>^><<>v^<<<<>^^v^<>>>^><^>>>^><v>v<>>^v<^>>><^^<v^vv^>^><<<>><><^v>^<>v<v>^<<^<^>^<>>>^v>><<^vv<^vv^v<>>v>v>><vv>v^^vv^<^^vv><^v<>^v>>vvv>v<<^<>^v^<vv^v<<>v^>vv<>^vv<^<^>><<>vvv^<>v<^<v>v>^^v^<v<<v^v^^^>vvvv><v<<v^vv<>vv^v^<^<<^v>v<^<<><<vv<>>v<^<^<<^>v<<^<<<>v><><^^>v<v<>v^>v>v><><<<^^<^>vv<v>><<>vv<<<>^v>>vvv^^v<^<v<v^v^^>><v<>^<^
<v<<v><vv<>^^v^v>^>vv^><^<>v><^vv^<^>>v<v>vvv<>>^vvv<>>v<>^v>><>vv>^v<v>vv^v^^<^v><<<vv<vv<v^<><v^><^>>v>vv^<^v^<^vvv><>><^^v<^<^<>>^>^<v>^<<v><<>v^vv>^v>><v^v^v<vv^^v^^vv<v<<v^v>^>^^v<<^^^>v<><><^>^>>v>>v<>^>>v><><v^<<v<>vv>v>>>^>^<>v^>vvv<^<vv><<>vv<<><v<<<><>^vvv<><<v>^^^v^^v>><<v<v>v>>v^v<vv<<<>>>^v<v>v>v>^v^<<<^>>^>^^^<<^>>^><v<^v^>v<><>^^<v>^^<>^vvvv^^v^<<<^><^><<^>v><^v^>v<<^><<vv<v><vv<<><<^^v<v>^^^><<v<>v>>^><v^<v>^vv<<<<^vv<v>v>>>^><><<<<v>v<^^v><>^v<^<>^<<v<^v^>vvv<<v>^^><><v<><^v<>>><<^>v><><v<>>><><^v<^<<><v^^v>vv>v>^^><^<^>v^>v>vv<>^<<^><v<<v><^^><v^>v<^v>^vvvv><^vv>^^^><^v><><vvv<<v^^^^vv>>v^>>v^v<>v>>><>v><>^<^<<<>>>>vv>>v<^>>><^<^>^^>><>^^<<<>v><v^^>v^<^v<<>vv>v^<v<^v><vv^<<>>^^<^><^<vvv>>vv^>>^<v>v><<><<>^v<><^^vv<^vv<<v>>><>>v>^<<^v<^v<^^>^><^>^^>><<<>^>>>>v>v>>>^v>^^v<^v>^<>^>>^<><^>vvv>>vv<<>^v^>^^>^<^^><^v<>>^v<><<>v>v^>^vv<vv<<v<^^><<v<v>^<<v>v<<v>><^v><v>^<^<<^<^v<^v>>v^^^v>v<v>>>^<^<v<^>v^v^v>><<^>^v>>v^<>v>^>^^<^v>>^v<^<^^><><<<<^vv<<^^>^><>>^>>>v>^^>^<v^^<>><
v<vv^>^<^^<><^^>>v>>v^^^<>v>v^<^<<v^^>>^>^>>^<v><^^^^>^<v>^v><vvv<^vv>v>>^><<^>>^vv<^^^^>^^v^<<v<><<vv<^<vv><v^v^v>v<^><<v^<<>^>><v<^v<<vvv><>>^vv^^<<^v<<>v><>^<<v>>v^v>^<<v><<v<^^>v>><vv<<^>><v<^>vv^vv<^<<v^vv<<^^>^^v><<^v>>>vv<^<^<v^^>^^v^>v><<>v^<>v><<v>v<v^vv^<^^>v<v^v>><^<<>>>v><^<v<><>^^<^<><^<<v>vvv<v^^vv^<^^><<v<vv^<<><<>>>^^^^>>v<^>>^v<v<^>v>vv<^>vvv><<v^>^^v<>^<>vvv^><v^<<<v<<><vv^<>>v<v^vv<><>v<<>v<^v^^<<v>^vvv>v<<v<v><<><v<^>>v^<^>v^vv^v>v^v<v<>><<>>>v<^<<^>vvv^>v>>>^vv><><<<v<>vvvv>^<>><^^v><><^v^v^^v<v<^vv^<>><^^>>>><^>^>^>v>>vvv<<>^><>^^v^^^v^^>v<<v>vv^><>><<v^<vv<><>v<v<^^vv<<><<vv^v<>v^><^v^^^^^<><<^<^^^>^^^vvv^<<^v>vv<<<v>v^<>v<<v^<<>^>v^vv^>^^>v<>vv^v><><>^<><>>><><^v^<>><<v<^v<<><v>vvvv<<^><v^><v>^v<><>v^<<^^vvv<<<>^>^>^^^>^v><^^<>^>^v<v^^v>^>>^v^v<<>vvvvv^<vv>^v<>v^<><^<^v<>^v>^^><^v>v<>v>vv>^>^v<^<<^>v^<v^<><v<vv>v>^^><vvvv>^<^>^vvv<<<^><^<v>^<^>>^^>^<<<>^>^vvv^v^<vv<^^v<v^^^>><v>^>><v>v<>><>><v<<<><^v^v^>>^^>>>v<<^^^>^v^^^><v><<<<<<^v><><vv<><^v>vvv>^v<v<v>v><>v<
<<>>v<^><^vv^^^^v^v^v^^<v<^v<<>^<^v<^^<<<>v^>>^^^v<>v^v>>>v<<><<<>^^>>^<<v>>v^<v>v^^^>^>^><>>><>>^v>><<^^vv>^<^^^<>^>>^<v>v<v^<>>>vvv^>v<v<><>^^v^<vv<>^^v>v<><<<>v^v>v<^v>^<^v^vv^><^>v^>v^>v^^^<>v>v<<v>v>v^^>><>^><v<<><<vv^>^vvv^v>^^<<<>^>v<<v<v<v>^<v>v<<>v>v<><>^^<v>>^vv<<v<^>><>vv>v<>v^>^^><v<<><^><><v<<>>><v^>v<^<v^v>><<<v<<v^><vv^^<<^<<>^<>v^><v<^v^v^<>>>^^^vv^<v^>><>vv<<vv>^<^v<v^^^><^>^>^<^v>><^v^v^>>^^v^v>v>>v>>vv^^^^>><>vv><vvv^v><<vv^<>>^^v><<<><vv>v^>^>^<^>^v>^<v>vv><><^<<<<^<^<v>^vv<<>>^v^^v^>>><vv>><^><v>>^<v>v>>v<v<v>><v<><v^<<^v^<v^v<<v^^<<><<^^v>v>>^<^^v>>vv<<^vv^^v<^>^^v<^^<<vv>>vv><^<v^^^<<^vv<<<v<^><<^><^^vv>^^>^^^<<v<v^><<<^v>>vv<vv^^><<<><^vv^><v>^<^<>>vvv^v>><^<vv>^^^>^v<>>vv^^<v<>^<>>^^v>>>^^vvvvv^<^<<>>v<vv^^<^<v><<>^<<>^<v^^^^>>>>v<v<><^^>v^><<><vvv^v<<vv<^v^v><><>^v^^v>>^<<v<v^^v^>>v<^<^^^v<<^>^^<<<<<^<>^><^<^v>^^^<>v^<>>^^>v^<^>^>^>^<<^><>v^v>>^<><^^>^>v<<v<><vv>>>v>>>^>>v<><<^<^v^<v^<v^<v^v>>>>v^<>^v<vv^^<>vv<<vv><<>>>^>^v^v><>>><vvv^<>^vv<<^^<>^v>><<v><<^<<^
vv<v>vv><><>>^^^>v><<>>><>^<^^^><v<vv^<<<><>><v^v^><>^<v^v^^v<<<v<v^<>>v><<<>>^<<^<v^<<<^>v<v^><<v^v<<^<<v><<^^^><v^>>>>^>>>>^><>v>><v<^^^>v>v>>v>><vv^vvvv>^^<>v><v^>v^^^^<<>vv<v><^>>>>^v>vv^>vvv^v<v>>>v<^^^>v>vvvv<<<vvv>^>^>><>v^<^^vv>^<v<^v><<><v^><<>v><<<><^v^^>><>>^^<v><><>^<<>>>^<>^<v>>^>><>^<<><vv^>^<v^v<><vv<^<<^v>^>><<>><^><<<>>v<>v^^>vvv<<^<>vv^>v<vv<>^><>^>^^v>^><><<v^<>v^v>v>^><><><^<v<^v>^^^>^v<^>^vv^^v<^><>>><^<<v<vv<v><^>>v><^v>>^>v>>v>>>vv><>v<^^v<<<^^<>^v><^<<v<<v<^^v><^^^^^>>vv^<<><>v<^<>vv<<>^^<>^>v<vv^^<^^^<v^>vv>^<<vv>v<v>>^^<>>>^vv^^^^^^><vvv>^><^^>v>^vv<<vv><><<>vv>v<v>^><><^>>v^>^>>v<v>vv<vv<^><v><<v<vv<v><><><v>^<>^<vv<^>^^v>^^^^^^v^>^<v<><^^^^<>><v>>>>>><<>>>>^<vv><<^>>>>>^>vv>^<<<<>v^><v>>>v^<vv^>vv^>>v>>v^^<><^<>v<^<v<<^<^><^><><><^^<^<><>^>v^<^>><>>>^^<vv<<vv>^^v<<vv^^vvvv^v^^<^v^v^^^^><v^<>^^v>vv<<^<>v<^>v><>^^^<<<^><<v>>v<<v<v><v^<v^vv^<vv<>^<^<>^>v>v<<^^^>>^^<>^<<^^<^^v^v^^v^^<^<<v>^<v>^^>^>^>v><^v^^^v>v^v>>v^>^>>v<>^<^v^^^^^<v>v<v^<^><^>>v^<^<<>^v^v^><<v"

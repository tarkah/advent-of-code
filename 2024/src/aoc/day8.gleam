import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

import aoc/day.{Day, Expects}

pub const day = Day(example, input, Expects(#(14, 34), #(364, 1231)), run)

fn run(input: String) {
  let map = parse(input)

  let part1 = antinodes(map, OneStep) |> set.size
  let part2 = antinodes(map, AllSteps) |> set.size

  #(part1, part2)
}

type Map {
  Map(width: Int, height: Int, antennas: List(Antenna))
}

type Antenna {
  Antenna(freq: String, pos: Pos)
}

type Pos {
  Pos(x: Int, y: Int)
}

type Calculation {
  OneStep
  AllSteps
}

fn antinodes(map: Map, calculation: Calculation) -> Set(Pos) {
  let by_freq =
    map.antennas
    |> list.fold_right(dict.new(), fn(acc, antenna) {
      dict.upsert(acc, antenna.freq, fn(value) {
        case value {
          None -> [antenna.pos]
          Some(xs) -> [antenna.pos, ..xs]
        }
      })
    })

  by_freq
  |> dict.to_list()
  |> list.flat_map(fn(a) {
    let #(_, positions) = a

    positions
    |> list.combination_pairs
    |> list.flat_map(fn(a) {
      let #(a, b) = a

      let a_run = enumerate(map, calculation, #(a.y - b.y, a.x - b.x), [a])
      let b_run = enumerate(map, calculation, #(b.y - a.y, b.x - a.x), [b])

      list.append(a_run, b_run)
    })
  })
  |> set.from_list
}

fn enumerate(
  map: Map,
  calculation: Calculation,
  slope: #(Int, Int),
  nodes: List(Pos),
) -> List(Pos) {
  case nodes {
    [] -> nodes
    [pos, ..] -> {
      let in_map = fn(pos: Pos) {
        pos.x >= 0 && pos.y >= 0 && pos.x < map.width && pos.y < map.height
      }
      let new = Pos(pos.x + slope.1, pos.y + slope.0)

      case calculation, in_map(new) {
        OneStep, False -> []
        OneStep, True -> [new]
        AllSteps, False -> nodes
        AllSteps, True -> enumerate(map, calculation, slope, [new, ..nodes])
      }
    }
  }
}

fn parse(input: String) -> Map {
  let lines = input |> string.split("\n")

  let height = lines |> list.length
  let width = lines |> list.first |> result.unwrap("") |> string.byte_size

  let map = Map(width, height, [])

  list.index_fold(lines, map, fn(map, line, y) {
    string.to_graphemes(line)
    |> list.index_fold(map, fn(map, char, x) {
      case char {
        "." -> map
        freq -> Map(..map, antennas: [Antenna(freq, Pos(x, y)), ..map.antennas])
      }
    })
  })
}

const example = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

const input = ".A...........5........................pL..........
.................................p......L.........
......................................L...........
.......................................C..........
........v...................7...............C.....
..................................p........L......
.................vA......3........................
.......A.....3....................................
........................s....X3...................
..A......5.................9....3.................
.......8...........s.........7.............C..m...
................8......t........7.......9.........
....................o......Z.............y........
...............s.......Y.v.o......y....0..........
..................................................
..5................8.......................m...J..
5...............................0....aX...........
.V............v.s.........Z.o..7....a.............
2..........f...........P..............9...J.M.....
...............f..........P.....V......y....1.J...
...g...................o.......0l...........N..B..
..................Y...............................
......G...............f.....Z..t..............1...
............G......Z......h................B....C.
.........w....h.Y....j............a........J..y...
.............P....z..........................1....
w.......P...z...R......r8.........................
........w.........................................
.................h.G.........m............BM......
......4.....fa.................G...i....X......W..
V........4..............................tW.9...i..
............2h..............0.......tX...M........
.....z.........................l..................
.......2..........................................
..r........................Y................W...i.
.......w.........q..................i.............
.........H.2....4.................................
..........Q.....j.......M.....lrN.................
..x...H.Q.......O.....c...........................
....H.......................S.....................
.....................O..S.......6..........b......
...c.......F...Q.j.........l....T.....R...........
...........Q.F.......c.I.....1.........R....T.....
............F........I.O......r..T.............b..
..n.........q.........F.I..............T..b.......
.......n...........z..O....x.......N........b.....
.....S............................................
..........q.........cS..x4I......6................
..j.....gn.q.......x...................N...6......
...........g..n................R......B..........."

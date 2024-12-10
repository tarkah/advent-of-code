import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set
import gleam/string

import aoc/day.{Day, Expects}

pub const day = Day(example, input, Expects(#(36, 81), #(512, 1045)), run)

fn run(input: String) {
  let map = parse(input)

  let part1 =
    map
    |> stats
    |> dict.values
    |> list.map(fn(stats) { stats.score })
    |> list.fold(0, int.add)

  let part2 =
    map
    |> stats
    |> dict.values
    |> list.map(fn(stats) { stats.rating })
    |> list.fold(0, int.add)

  #(part1, part2)
}

type Map {
  Map(width: Int, height: Int, heights: BitArray, trailheads: List(Pos))
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

type Stats {
  Stats(score: Int, rating: Int)
}

// calculate stats for each trailhead
fn stats(map: Map) -> Dict(Pos, Stats) {
  stats_loop(map, map.trailheads, dict.new())
}

fn stats_loop(
  map: Map,
  trailheads: List(Pos),
  stats: Dict(Pos, Stats),
) -> Dict(Pos, Stats) {
  case trailheads {
    [trailhead, ..rest] -> {
      let tops = trailhead_tops(map, trailhead, 0, [])
      // Total number of trails hitting each top
      let rating = tops |> list.length
      // Total unique tops hits
      let score = tops |> set.from_list |> set.size

      stats_loop(map, rest, dict.insert(stats, trailhead, Stats(score, rating)))
    }
    [] -> stats
  }
}

// Get all 9 positions visited
fn trailhead_tops(map: Map, pos: Pos, height: Int, tops: List(Pos)) -> List(Pos) {
  [Up, Down, Left, Right]
  |> list.fold(tops, fn(tops, dir) {
    let next = move(pos, dir)

    case get_height(map, next) {
      Ok(next_height) -> {
        case next_height - height {
          1 if next_height == 9 -> [next, ..tops]

          1 -> trailhead_tops(map, next, next_height, tops)

          _ -> tops
        }
      }

      _ -> tops
    }
  })
}

fn move(pos: Pos, dir: Direction) -> Pos {
  case dir {
    Up -> Pos(..pos, y: pos.y + 1)
    Down -> Pos(..pos, y: pos.y - 1)
    Left -> Pos(..pos, x: pos.x - 1)
    Right -> Pos(..pos, x: pos.x + 1)
  }
}

fn in_bounds(map: Map, pos: Pos) -> Bool {
  let Pos(x, y) = pos

  x >= 0 && y >= 0 && x < map.width && y < map.height
}

fn get_height(map: Map, pos: Pos) -> Result(Int, Nil) {
  use <- bool.guard(when: !in_bounds(map, pos), return: Error(Nil))

  case bit_array.slice(map.heights, pos.y * map.width + pos.x, 1) {
    Ok(<<height>>) -> Ok(height)
    _ -> Error(Nil)
  }
}

fn parse(input: String) -> Map {
  let lines = input |> string.split("\n") |> list.map(string.to_graphemes)

  let width = lines |> list.first |> result.unwrap([]) |> list.length
  let height = lines |> list.length

  let map = Map(width, height, <<>>, [])

  lines
  |> list.index_fold(map, fn(map, line, y) {
    line
    |> list.index_fold(map, fn(map, char, x) {
      let assert Ok(height) = int.parse(char)

      let heights = bit_array.append(map.heights, <<height:int>>)

      let trailheads = case height == 0 {
        False -> map.trailheads
        True -> [Pos(x, y), ..map.trailheads]
      }

      Map(..map, heights: heights, trailheads: trailheads)
    })
  })
}

const example = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

const input = "78434565658934341239890154327898789410169567876
89125676543823430123763267016505654321678478965
74034389012710569834354108987419783210501329450
65985293405613478765587017096328798193432010321
54876102564302349323498723165437689087589876501
03123001273211058010567654232126575670670345432
12054320985670769623458912343071464321561210894
23065011234987878543467801056780352143254308743
52176020143010987632966532963091243034165789652
43982176542123676701876547872108389435045630001
04343987233034565899892101543219474326554321100
15458980154901454300765413256978365217893033234
26967898067872343211234322107863210105676128744
37810587120143443205895013278954306018985439653
45721456431234556106786784567805217897851058912
96012367589109667676632397876516986786542367803
87183398676008768985541098923427875987034456934
45698432195419878104323567012434564100124325965
34787563084328769012013432100123473236787619876
23456976176101098743100169981210984345894500761
10067885105432345654221058974303876201903121450
00198793234569854783334567565012565102812034321
87235630321478345698448987545643476983456965410
96544321410145430789567496538753985876589876521
87875401521034521876321323429832104367674307834
76965432690123670965410210018943011278765212985
10126501785434987012124567877654780569890156676
67635652376501456921023498965345698430732347787
58548743210122357830010329453276582521231298898
19989834543218766545689414340189891234560106787
05678129670109658998776504276567760143671015890
14329078786788347643210543183498452087982384321
23010879895690210556987652092354301096543493430
10123965654321987467878941001289212332102584321
89854434365432176307650030110176565443001675610
78765601278300045218941121230145896558903498701
01251012369211234567132430548234787367812345672
14344323054334565478012986699876572210787434987
65689454120423672389873677780125601923896523654
56788763201210981014564578456234892854589018743
45697899854327872103434569327844763765474329012
34307656761056932112363231218903454892365215603
43218947892343341001454120305412178901054508763
52107232103217654012360019456543065016543459454
67800134564308567897871298787832104327872106563
58910129875699430108998345690965432112967654312
67623456766780123234567654321678987003458901203"

import gleam/bit_array
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string

import aoc/day.{Day, Expects}

// Example: 5,7,3,0           == 15
// Input:   1,7,2,1,4,1,5,4,0 == 25
pub const day = Day(
  example,
  input,
  Expects(#(15, 117_440), #(25, 37_221_261_688_308)),
  run,
)

fn run(input: String) {
  let machine = parse(input)

  let output = run_program(machine)

  // Value we submit to AOC
  output |> list.map(int.to_string) |> string.join(",") |> io.debug

  // Add them together for assertions
  let part1 =
    output
    |> list.fold(0, int.add)

  let part2 = debug(machine)

  #(part1, part2)
}

type Machine {
  Machine(
    register_a: Int,
    register_b: Int,
    register_c: Int,
    instruction: Int,
    program: BitArray,
  )
}

type Opcode {
  ADV
  BXL
  BST
  JNZ
  BXC
  OUT
  BDV
  CDV
}

type Operand {
  NoOperand
  Literal(Int)
  RegA
  RegB
  RegC
}

type OperandType {
  OpCombo
  OpLiteral
  OpNone
}

type Instruction {
  Instruction(opcode: Opcode, operand: Operand)
}

type NextInstruction {
  Halt
  NextInstruction(Instruction)
}

type Output =
  List(Int)

fn debug(machine: Machine) -> Int {
  debug_loop(machine, 0, 1, 0)
}

// Shift reg_a by 3 bits for each output and add 1 until 
// we get the expected output for that index
fn debug_loop(machine: Machine, reg_a: Int, i: Int, n: Int) -> Int {
  let new_reg_a = reg_a |> int.bitwise_shift_left(3) |> int.bitwise_or(n)

  let program = machine.program |> program_to_array([])

  let assert Ok(expected) =
    machine.program
    |> bit_array.slice(machine.program |> bit_array.byte_size, -i)
    |> result.map(program_to_array(_, []))

  let output = run_program(Machine(..machine, register_a: new_reg_a))

  case output == expected {
    True ->
      case output == program {
        True -> new_reg_a
        False -> debug_loop(machine, new_reg_a, i + 1, 0)
      }
    False -> debug_loop(machine, reg_a, i, n + 1)
  }
}

fn program_to_array(program: BitArray, out: List(Int)) -> List(Int) {
  case program {
    <<a:int, rest:bits>> -> {
      program_to_array(rest, [a, ..out])
    }
    _ -> out |> list.reverse
  }
}

fn run_program(machine: Machine) -> Output {
  run_program_loop(machine, [])
}

fn run_program_loop(machine: Machine, output: Output) -> Output {
  let #(machine, instruction) = next_instruction(machine)

  case instruction {
    Halt -> output |> list.reverse

    NextInstruction(instruction) -> {
      let #(machine, output) = process(machine, output, instruction)

      run_program_loop(machine, output)
    }
  }
}

fn process(
  machine: Machine,
  output: Output,
  instruction: Instruction,
) -> #(Machine, Output) {
  let Instruction(opcode, operand) = instruction

  case opcode {
    ADV -> {
      let assert Ok(pow) =
        2
        |> int.power(op_value(machine, operand) |> int.to_float)

      let assert Ok(value) =
        machine.register_a |> int.to_float |> float.divide(pow)

      let machine = Machine(..machine, register_a: value |> float.truncate)

      #(machine, output)
    }
    BXL -> {
      let value =
        int.bitwise_exclusive_or(machine.register_b, op_value(machine, operand))

      let machine = Machine(..machine, register_b: value)

      #(machine, output)
    }
    BST -> {
      let value = op_value(machine, operand) % 8

      let machine = Machine(..machine, register_b: value)

      #(machine, output)
    }
    JNZ ->
      case machine.register_a {
        0 -> #(machine, output)
        _ -> #(
          Machine(..machine, instruction: op_value(machine, operand)),
          output,
        )
      }
    BXC -> {
      let value =
        int.bitwise_exclusive_or(machine.register_b, machine.register_c)

      let machine = Machine(..machine, register_b: value)

      #(machine, output)
    }
    OUT -> {
      let value = op_value(machine, operand) % 8

      #(machine, [value, ..output])
    }
    BDV -> {
      let assert Ok(pow) =
        2
        |> int.power(op_value(machine, operand) |> int.to_float)

      let assert Ok(value) =
        machine.register_a |> int.to_float |> float.divide(pow)

      let machine = Machine(..machine, register_b: value |> float.truncate)

      #(machine, output)
    }
    CDV -> {
      let assert Ok(pow) =
        2
        |> int.power(op_value(machine, operand) |> int.to_float)

      let assert Ok(value) =
        machine.register_a |> int.to_float |> float.divide(pow)

      let machine = Machine(..machine, register_c: value |> float.truncate)

      #(machine, output)
    }
  }
}

fn next_instruction(machine: Machine) -> #(Machine, NextInstruction) {
  let op_type = fn(opcode) {
    case opcode {
      ADV -> OpCombo
      BXL -> OpLiteral
      BST -> OpCombo
      JNZ -> OpLiteral
      BXC -> OpNone
      OUT -> OpCombo
      BDV -> OpCombo
      CDV -> OpCombo
    }
  }

  case bit_array.slice(machine.program, machine.instruction, 2) {
    Ok(<<opcode:int, operand:int>>) -> {
      let opcode = case opcode {
        0 -> ADV
        1 -> BXL
        2 -> BST
        3 -> JNZ
        4 -> BXC
        5 -> OUT
        6 -> BDV
        7 -> CDV
        _ -> panic as "invalid opcode"
      }

      let operand = case op_type(opcode) {
        OpCombo ->
          case operand {
            0 -> Literal(0)
            1 -> Literal(1)
            2 -> Literal(2)
            3 -> Literal(3)
            4 -> RegA
            5 -> RegB
            6 -> RegC
            7 -> panic as "operand is reserved"
            _ -> panic as "invalid operand"
          }
        OpLiteral -> Literal(operand)
        OpNone -> NoOperand
      }

      #(
        Machine(..machine, instruction: machine.instruction + 2),
        NextInstruction(Instruction(opcode, operand)),
      )
    }

    _ -> #(machine, Halt)
  }
}

fn op_value(machine: Machine, operand: Operand) -> Int {
  case operand {
    Literal(a) -> a
    NoOperand -> 0
    RegA -> machine.register_a
    RegB -> machine.register_b
    RegC -> machine.register_c
  }
}

fn parse(input: String) -> Machine {
  let assert Ok(#(top, bottom)) = input |> string.split_once("\n\n")

  let assert Ok(#(_, program)) = bottom |> string.split_once(": ")
  let assert Ok(program) =
    program
    |> string.split(",")
    |> list.try_map(int.parse)
    |> result.map(list.fold(_, <<>>, fn(acc, int) { <<acc:bits, int:int>> }))

  let parse_register = fn(line) {
    let assert Ok(#(_, rhs)) = line |> string.split_once(": ")
    let assert Ok(a) = int.parse(rhs)
    a
  }

  let assert [a, b, c] =
    top
    |> string.split("\n")

  Machine(parse_register(a), parse_register(b), parse_register(c), 0, program)
}

const example = "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"

const input = "Register A: 27575648
Register B: 0
Register C: 0

Program: 2,4,1,2,7,5,4,1,1,3,5,5,0,3,3,0"

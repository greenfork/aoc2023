use "files"
use "itertools"

actor Main
  new create(env: Env) =>
    let example = """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """

    var sum: U32 = 0
    var sum2: U32 = 0

    // for line' in example.split("\n").values() do
    //   let line: String val = consume line'
    //   let int2 = find_int2(line, env.out)
    //   sum2 = sum2 + int2
    // end

    try
      with file = OpenFile(FilePath(FileAuth(env.root), "input")) as File do
        for line' in file.lines() do
          let line: String val = consume line'
          try
            let int = find_int(line)?
            sum = sum + int
          else
            env.err.print("Couldn't find int")
          end
          let int2 = find_int2(line, env.out)
          sum2 = sum2 + int2
        end
      end
    else
      env.err.print("Failed to open a file")
    end
    env.out.print(sum.string())
    env.out.print(sum2.string())

  fun find_int(line: String box): U32 ? =>
    var first: (U32 | None) = None
    var last: U32 = 0
    for ch in line.values() do
      match ch
      | let ch': U8 if (ch' >= '0') and (ch' <= '9') =>
        let number = (ch' - '0').u32()
        if first is None then first = number end
        last = number
      end
    end
    ((first as U32) * 10) + last

  fun find_int2(line: String box, out: OutStream): U32 =>
    var first: U32 = 0
    var last: U32 = 0
    var min_idx: ISize = ISize.max_value()
    var max_idx: ISize = -1
    for pattern in ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"
                    "zero"; "one"; "two"; "three"; "four"; "five"; "six"
                    "seven"; "eight"; "nine"].values() do
      try
        let min_idx' = line.find(pattern)?
        if min_idx' < min_idx then
          min_idx = min_idx'
          first = pattern_to_int(pattern, out)
        end
      end
      try
        let max_idx' = line.rfind(pattern)?
        if max_idx' > max_idx then
          max_idx = max_idx'
          last = pattern_to_int(pattern, out)
        end
      end
    end
    // out.print(line + " resulted in " + first.string() + last.string())
    (first * 10) + last

  fun pattern_to_int(pattern: String box, out: OutStream): U32 =>
    match pattern
    | "0" => 0
    | "1" => 1
    | "2" => 2
    | "3" => 3
    | "4" => 4
    | "5" => 5
    | "6" => 6
    | "7" => 7
    | "8" => 8
    | "9" => 9
    | "zero" => 0
    | "one" => 1
    | "two" => 2
    | "three" => 3
    | "four" => 4
    | "five" => 5
    | "six" => 6
    | "seven" => 7
    | "eight" => 8
    | "nine" => 9
    else
      out.print("Failed to convert " + pattern)
      0
    end

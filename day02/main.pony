use "collections"
use "files"
use "peg"

primitive MyParser
  fun apply(): Parser val =>
    recover
      let digit = R('0', '9')
      let digits = digit.many1().term(MNumber)
      let color = (L("red") / L("green") / L("blue")).term(MColor)
      let stone = (digits * color).node(MStone)
      let try' = stone.many(L(",")).node(MTry) * -(L(";") / L("\n"))
      let game = (-L("Game") * digits * -L(":") * try'.many1()).node(MGame)
      let start = game.many1()
      let hidden = L(" ").many()
      start.hide(hidden)
    end

primitive MNumber is Label fun text(): String => "Number"
primitive MColor is Label fun text(): String => "Color"
primitive MStone is Label fun text(): String => "Stone"
primitive MTry is Label fun text(): String => "Try"
primitive MGame is Label fun text(): String => "Game"

class Game
  let number: U32
  embed tries: Array[Stones] = tries.create()

  new create(ast: AST) ? =>
    number = (ast.children(0)? as Token).string().u32()?
    for i in Range(1, ast.children.size()) do
      let try' = ast.children(i)? as AST
      var stones = Stones
      for stone' in try'.children.values() do
        let stone = stone' as AST
        let number' = (stone.children(0)? as Token).string().u32()?
        let color = (stone.children(1)? as Token).string()
        match color
        | "red" => stones.red = number'
        | "green" => stones.green = number'
        | "blue" => stones.blue = number'
        else error
        end
      end
      tries.push(stones)
    end

  fun string(): String val =>
    var s = "Game " + this.number.string() + ":"
    for stones in this.tries.values() do
      s =
        s
        + " red " + stones.red.string() + ", "
        + " green " + stones.green.string() + ", "
        + " blue " + stones.blue.string() + ";"
    end
    s

  fun is_possible_with(bag: Stones): Bool =>
    for stones in this.tries.values() do
      if (stones.red > bag.red) or
         (stones.green > bag.green) or
         (stones.blue > bag.blue) then
       return false
      end
    end
    true

  fun minimal_bag(): Stones =>
    var bag = Stones
    for stones in this.tries.values() do
      bag.red = bag.red.max(stones.red)
      bag.green = bag.green.max(stones.green)
      bag.blue = bag.blue.max(stones.blue)
    end
    bag

class Stones
  var red: U32
  var green: U32
  var blue: U32

  new create(red': U32 = 0, green': U32 = 0, blue': U32 = 0) =>
    red = red'
    green = green'
    blue = blue'

  fun power(): U32 => this.red * this.green * this.blue

actor Main
  new create(env: Env) =>
    let example = """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """

    let input =
      try
        with file = OpenFile(FilePath(FileAuth(env.root), "input")) as File do
          file.read_string(file.size())
        end
      else
        env.err.print("Failed to open a file")
        return
      end

    let possible_bag = Stones(12, 13, 14)
    var game_id_sum: U32 = 0
    var sum_of_powers: U32 = 0

    try
      let ast = parse(MyParser(), consume input, env.out)? as AST
      for game' in ast.children.values() do
        let game = Game(game' as AST)?
        // First task
        if game.is_possible_with(possible_bag) then
          game_id_sum = game_id_sum + game.number
        end

        // Second task
        sum_of_powers = sum_of_powers + game.minimal_bag().power()
      end
    else
      env.err.print("Error parsing AST")
    end
    env.out.print(game_id_sum.string())
    env.out.print(sum_of_powers.string())

  fun parse(p: Parser val, target_source: String, out: OutStream) : ASTChild ? =>
    let target = Source.from_string(target_source, "target.input")
    match recover val p.parse(target) end
    | (_, let r: ASTChild) =>
      return r
    | (let offset: USize, let r: Parser val) =>
      let e = recover val SyntaxError(target, offset, r) end
      out.writev(PegFormatError.console(e, false))
    end
    error



val lines = scala.io.Source.fromFile("input.txt").getLines.toList

sealed trait RawInstruction
case class AddX(value: Int) extends RawInstruction
case object Noop extends RawInstruction

def parseInstruction(line: String): RawInstruction = {
  val op = line.take(4)

    op match {
        case "noop" => Noop
        case "addx" => AddX(line.drop(5).toInt)
    }
}

val instructions = lines.map(parseInstruction)

sealed trait MicroCode
case class Add(value: Int) extends MicroCode
case object Sleep1 extends MicroCode

def compileInstruction(instruction: RawInstruction): List[MicroCode] = {
    instruction match {
        case AddX(value) => List(Sleep1, Add(value), Sleep1)
        case Noop => List(Sleep1)
    }
}

val microCode = instructions.flatMap(compileInstruction)

case class State[S](register: Int, cycle: Int, snapshot: S)
def newState[S](initialSnapshot: S): State[S] = State(1, 1, initialSnapshot)

def runMicroCode[S](microCode: MicroCode, state: State[S], takeSnapshot: State[S] => S): State[S] = {
    microCode match {
        case Add(value) => state.copy(register = state.register + value)
        case Sleep1 =>
            val newState = state.copy(cycle = state.cycle + 1)
            newState.copy(snapshot = takeSnapshot(newState))
    }
}

def runAllMicroCode[S](microCodes: List[MicroCode], state: State[S], takeSnapshot: State[S] => S): State[S] = {
    microCodes.foldLeft(state.copy(snapshot = takeSnapshot(state)))((state, microCode) => runMicroCode(microCode, state, takeSnapshot))
}

type PowerSnapshot = Map[Int, Int]

def registerPower(state: State[PowerSnapshot]): PowerSnapshot = state.snapshot + (state.cycle -> state.register * state.cycle)

val powerStates = runAllMicroCode(microCode, newState(Map()), registerPower)
println(powerStates.snapshot(20))
println(powerStates.snapshot(60))
println(powerStates.snapshot(100))
println(powerStates.snapshot(140))
println(powerStates.snapshot(180))
println(powerStates.snapshot(220))
for (i <- 1 to 240) {
    println(s"$i * ${powerStates.snapshot(i) / i} => ${powerStates.snapshot(i)}")
}
val part1 = powerStates.snapshot(20) + powerStates.snapshot(60) + powerStates.snapshot(100) + powerStates.snapshot(140) + powerStates.snapshot(180)  + powerStates.snapshot(220)

type RawSnapshot = Map[Int, Int]

def registerRaw(state: State[RawSnapshot]): RawSnapshot = state.snapshot + (state.cycle -> state.register)

val rawStates = runAllMicroCode(microCode, newState(Map()), registerRaw)

for {
    i <- 1 to 240
    cycle = i
    pos = cycle % 40
    register = rawStates.snapshot(i)
} {
    (pos - register) match {
        case 0 | 1 | 2 => print("#")
        case _ => print(".")
    }
    if (pos == 0) {
        println()
    }
}
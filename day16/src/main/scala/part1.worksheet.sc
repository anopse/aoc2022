import scala.util.parsing.combinator._
import scala.annotation.tailrec

val isSampleInput = false
val inputFile = if isSampleInput then "sample.txt" else "input.txt"
val input = scala.io.Source.fromFile(inputFile).mkString

// Input format:
// Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
// Valve BB has flow rate=13; tunnels lead to valves CC, AA

sealed case class ValveDesc(name: String, flowRate: Int, tunnels: List[String])

object Parser extends RegexParsers with PackratParsers {
    def name: Parser[String] = """\w+""".r
    def flowRate: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def tunnels: Parser[List[String]] = rep1sep(name, ",")
    def valve: Parser[ValveDesc] = "Valve" ~> name ~ ("has flow rate=" ~> flowRate) ~ (";" ~> ("tunnels lead to valves" | "tunnel leads to valve") ~> tunnels) ^^ { case n ~ f ~ t => ValveDesc(n, f, t) }
    def problem: Parser[List[ValveDesc]] = rep1sep(valve, "")
}

val rawProblem = Parser.parseAll(Parser.problem, input)
val problem = rawProblem.get

val nameToId = problem.zipWithIndex.map(kvp => kvp._1.name -> kvp._2).toMap
val idToName = nameToId.map(_.swap)

sealed case class Tunnel(to: Int, tick: Int)

sealed case class Valve(id: Int, flowRate: Int, tunnels: List[Tunnel])

val valves = problem.map(v => Valve(nameToId(v.name), v.flowRate, v.tunnels.map(s => Tunnel(nameToId(s), 1)))).toArray

final def expectedRelease(valve: Valve, timeRemaining: Int): Int = valve.flowRate * timeRemaining


val optimizedValves = valves.clone()
val NilValve = Valve(-1, 0, Nil)

def removeValve(rmId: Int): Unit = {
    val rmValve = optimizedValves(rmId)
    optimizedValves.mapInPlace(v => {
       if (v.id == rmId) {
           NilValve
       } else {
           val newTunnels = v.tunnels.flatMap(t => 
               if (t.to == rmId) {
                   val rawTunnels = rmValve.tunnels.map(vt => vt.copy(tick = vt.tick + t.tick))
                   val withoutItself = rawTunnels.filter(_.to != v.id)
                   withoutItself
               } else List(t)
           )

           val withoutSlower = newTunnels.groupBy(_.to).view.mapValues(_.minBy(_.tick)).map(_._2).toList

           v.copy(tunnels = withoutSlower)
       }
   })
}

val initialId = nameToId("AA")

// removeValve(nameToId("FF"))
// removeValve(nameToId("GG"))

for {
    i <- 0 until optimizedValves.length
} {
    if (optimizedValves(i).flowRate == 0 && i != initialId) {
        removeValve(i)
    }
}

for v <- valves do println(s"Valve ${idToName(v.id)} has flow rate=${v.flowRate}; tunnels lead to valves ${v.tunnels.map(t => s"${idToName(t.to)}(${t.tick})").mkString(", ")}")
for v <- optimizedValves.filter(_.id != -1) do println(s"Valve ${idToName.getOrElse(v.id,"-1")} has flow rate=${v.flowRate}; tunnels lead to valves ${v.tunnels.map(t => s"${idToName.getOrElse(t.to,"-1")}(${t.tick})").mkString(", ")}")


sealed case class State(opened: Long, timeRemaining: Int, currentValve: Int, totalRelease: Int) {
    def isOpen(id: Int): Boolean = (opened & (1L << id)) != 0
    def release: State = {
        val newTimeRemaining = timeRemaining - 1
        copy(totalRelease = totalRelease + expectedRelease(optimizedValves(currentValve), newTimeRemaining), opened = opened | (1L << currentValve), timeRemaining = newTimeRemaining)
    }
    def changeLocation(tunnel: Tunnel): State = copy(currentValve = tunnel.to, timeRemaining = timeRemaining - tunnel.tick)

    def gotoAndOpen(tunnel: Tunnel): State = changeLocation(tunnel).release
    def justGoto(tunnel: Tunnel): State = changeLocation(tunnel)

    def genericGoto(tunnel: Tunnel): List[State] = if isOpen(tunnel.to) then List(justGoto(tunnel)) else List(gotoAndOpen(tunnel), justGoto(tunnel))
}

val maxValve = optimizedValves.length - 1

final def computeBestPossibleRelease(state: State): Int = {
    var maxExpectedRelease = state.totalRelease

    for {
        id <- 0 to maxValve
    } {
        if !state.isOpen(id) then {
            val valve = optimizedValves(id)
            val release = expectedRelease(valve, state.timeRemaining)
            maxExpectedRelease += release
        }
    }

    maxExpectedRelease
}

@tailrec
final def solve(toVisit: List[State], accBestState: State, timeToStop: Int): State = {
    toVisit match
        case state :: next =>
            if (state.timeRemaining > timeToStop) {
                val nextStates = optimizedValves(state.currentValve).tunnels.flatMap(t => state.genericGoto(t))
                val filteredNextStates = nextStates.filter(s => computeBestPossibleRelease(s) > accBestState.totalRelease)
                solve(filteredNextStates ++ next, accBestState, timeToStop)
            } else {
                if (state.totalRelease > accBestState.totalRelease) {
                    val newBestState = state
                    val newNext = next.filter(s => computeBestPossibleRelease(s) > newBestState.totalRelease)
                    solve(next, newBestState, timeToStop)
                } else {
                    solve(next, accBestState, timeToStop)
                }
            }
        case Nil => accBestState
}


val initialTime = 30
val initialState = State(0, initialTime, initialId, 0)
//solve(List(initialState), initialState, 29)

// val test = initialState
//             .gotoAndOpen(Tunnel(nameToId("DD"), 1))
//             .justGoto(Tunnel(nameToId("CC"), 1))
//             .gotoAndOpen(Tunnel(nameToId("BB"), 1))
//             .justGoto(Tunnel(nameToId("AA"), 1))
// //            .justGoto(nameToId("II"))
//             .gotoAndOpen(Tunnel(nameToId("JJ"), 2))
// //            .justGoto(nameToId("II"))
//             .justGoto(Tunnel(nameToId("AA"), 2))
//             .justGoto(Tunnel(nameToId("DD"), 1))
//             .justGoto(Tunnel(nameToId("EE"), 1))
// //            .justGoto(nameToId("FF"))
// //            .justGoto(nameToId("GG"))
//             .gotoAndOpen(Tunnel(nameToId("HH"), 3))
// //            .justGoto(nameToId("GG"))
// //            .justGoto(nameToId("FF"))
//             .gotoAndOpen(Tunnel(nameToId("EE"), 3))
//             .justGoto(Tunnel(nameToId("DD"), 1))
//             .gotoAndOpen(Tunnel(nameToId("CC"), 1))

@tailrec
final def iterFind(currentDepth: Int, bestFound: State): State = {
    val newBest = solve(List(initialState), bestFound, currentDepth)

    if (currentDepth < 0) {
        bestFound
    } else {
        if (newBest.totalRelease > bestFound.totalRelease) {
            println(s"Found better solution at depth $currentDepth: ${newBest.totalRelease}")
            iterFind(currentDepth - 1, newBest)
        } else {
            println(s"Found no better solution at depth $currentDepth")
            iterFind(currentDepth - 1, bestFound)
        }
    }
    
}

val finalState = iterFind(initialTime, initialState)

// for id <- finalState.visited.toBinaryString.reverse.zipWithIndex.filter(_._1 == '1').map(_._2) do
//     println(idToName(id))
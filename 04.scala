import aoc.time

enum CellState:
    case empty, paper, removed

    def char: Char = this match {
        case CellState.empty   => '.'
        case CellState.paper   => '@'
        case CellState.removed => 'x'
    }

object CellState:
    def fromCharacter(character: Char): CellState =
        character match {
            case '@' => CellState.paper
            case '.' => CellState.empty
            case 'x' => CellState.removed
            case _   =>
                assert(
                  false,
                  f"You bumbling buffoon, '$character' is not allowed"
                )
        }

case class Cell(
    y: Int,
    x: Int,
    state: CellState
):
    def containsPaper: Boolean = state == CellState.paper
    def empty: Boolean = !containsPaper

    def afterRemoval: Cell =
        Cell(y, x, CellState.removed)

    def neighborsInWorld(world: World): List[Cell] =
        List(y - 1, y, y + 1)
            .flatMap(y => List(x - 1, x, x + 1).map(x => (y, x)))
            .filter((yNeighbor, xNeighbor) => (yNeighbor, xNeighbor) != (y, x))
            .filter((y, x) =>
                0 <= y && y < world.length && 0 <= x && x < world(y).length
            )
            .map((y, x) => world(y)(x))

    def accessibleInWorld(world: World): Boolean =
        containsPaper && (neighborsInWorld(world).count(_.containsPaper) < 4)

type World = List[List[Cell]]

implicit class WorldWithCells(world: World):
    def accessibleCellCount: Int =
        world.flatten.count(_.accessibleInWorld(world))

    def transform(): (World, Int) =
        var removedCells = 0
        val newWorld = world.map(line =>
            line.map(cell =>
                val canBeRemoved = cell.accessibleInWorld(world)
                if canBeRemoved then
                    removedCells = removedCells + 1
                    cell.afterRemoval
                else cell
            )
        )
        (newWorld, removedCells)

    def pprint(): Unit = println(
      world.map(_.map(_.state.char).mkString).mkString("\n")
    )

@main def aoc25x04(): Unit =
    val source = scala.io.Source.fromFile("inputs/04.txt")
    val realInput =
        try source.mkString
        finally source.close()
    val dummyInput =
        "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@."

    val input = realInput

    val world: World = time("parse"):
        input
            .split("\n")
            .filter(_.length > 0)
            .zipWithIndex
            .map((line, y) =>
                line.zipWithIndex
                    .map((c, x) => Cell(y, x, CellState.fromCharacter(c)))
                    .toList
            )
            .toList

    time("part1"):
        world.transform()._1

    time("part2"):
        Iterator
            .iterate((world, 0))((world, taken) => world.transform())
            .drop(1)
            .takeWhile((_, taken) => taken > 0)
            // .map((world, taken) =>
            //     world.pprint()
            //     println("---------------")
            //     (world, taken)
            // )
            .map((world, taken) => taken)
            .sum

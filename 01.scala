import Math.{floorMod, floorDiv}

// Add .zeros to Iterable of Integers
implicit class ZeroCountingIntIterable(items: Iterable[Int]):
    def zeros: Int = items.count(_ % 100 == 0)

// Add .last to Sequences
implicit class EndingIterable[T](items: Seq[T]):
    def last: T = items(items.length - 1)

// Data structure that contains a result and an execution time
case class TimedExecution[T](result: T, nanoSeconds: Double):
    def microseconds = nanoSeconds / 1_000.0
    def milliseconds = nanoSeconds / 1_000_000.0
    def seconds = nanoSeconds / 1_000_000_000.0
    // Pretty string format duration
    def prettyDuration: String = this match
        case _ if nanoSeconds < 100.0  => f"$nanoSeconds%.2f ns"
        case _ if microseconds < 100.0 => f"$microseconds%.2f µs"
        case _ if milliseconds < 100.0 => f"$milliseconds%.2f ms"
        case _                         => f"$seconds%.2f s"

    override def toString: String =
        if result.isInstanceOf[Int] then f"$prettyDuration ==> $result"
        else prettyDuration

object TimedExecution:
    def timing[T](block: => T): TimedExecution[T] =
        val before = System.nanoTime
        val result = block
        val after = System.nanoTime
        TimedExecution(result, after - before)

// Time a block and print a message
def time[T](message: String)(block: => T): T =
    val result = TimedExecution.timing(block)
    println(f"$message $result")
    result.result

def wrap = (number: Int) => floorMod(number, 100)

@main def aoc25x01(): Int =
    val source = scala.io.Source.fromFile("inputs/01.txt")
    val lines =
        try source.getLines.toList
        finally source.close()

    // val lines =
    // List("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")

    var rotations = time("parse")(
      lines
          .filter(_.length() > 1)
          .map(line => (if line(0) == 'L' then -1 else 1) * line.drop(1).toInt)
    )

    // part 1
    time("part1"):
        rotations
            .scanLeft(50)((current, rotation) => wrap(current + rotation))
            .zeros

    // BEGIN part 2
    // For all rotations call (lastValue, newValue, rotation) and expect the number of zeros
    def accumulateForAllRotations(getZeros: (Int, Int, Int) => Int): Int =
        rotations
            .foldLeft((50, 0))((folded: (Int, Int), rotation: Int) =>
                val (lastValue, lastZeros) = folded
                val newValue = lastValue + rotation
                (newValue, lastZeros + getZeros(lastValue, newValue, rotation))
            )
            ._2

    // which nth rotation are you in (including 0 and not 100)
    def sector = (value: Int) => floorDiv(value, 100)
    // my beautiful boi
    time("part2 beautiful"):
        // count 0 crossings. This however only works reliably counting up.
        // 0 -> 50 works. No sector is crossed
        // 0 -> 150 crosses one sector boundary (@100) so it counts as 1
        // 50 -> 0 registers as 0 since it remains in the same sector.
        //    However it should be one
        // So when rotating down, the dial is mirrored. +10 becomes -10 and so on.
        // Thus the 50 -> 0 rotation becomes -50 -> 0 which crosses from sector -1 to 0
        accumulateForAllRotations(
          (lastValue: Int, newValue: Int, rotation: Int) =>
              val mirror = rotation.sign
              sector(mirror * newValue) - sector(mirror * lastValue)
        )

    // all the values from a starting value (exclusive) and an offset
    def valuesRotating(startingValue: Int, rotation: Int) =
        (startingValue to startingValue + rotation by rotation.sign).drop(1)
    // list with all values to append to an existing list
    def nextList(lastList: List[Int], rotation: Int) =
        valuesRotating(lastList.last, rotation).toList

    // generate all values the dial passes though, count the zeros and sum them
    time("part2 meh boiii"):
        accumulateForAllRotations(
          (lastValue: Int, newValue: Int, rotation: Int) =>
              valuesRotating(lastValue, rotation).zeros
        )

    // Generates a list of each position, flattens them and counts zeros
    time("part2 kindaugly"):
        rotations.scanLeft(List(50))(nextList).flatten.zeros

    // he is a ugly boi
    // ================
    // Extend the list every time and pass it on.
    // Performs list concatenation every time
    time("part2 ugly boii"):
        rotations
            .foldLeft(List(50))((lastList: List[Int], rotation: Int) =>
                lastList ::: nextList(lastList, rotation)
            )
            .zeros

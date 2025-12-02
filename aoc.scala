package aoc

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
        if result.isInstanceOf[Int] || result.isInstanceOf[Long] then
            f"$prettyDuration ==> $result"
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

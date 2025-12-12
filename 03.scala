package aoc25x03

import aoc.time
import math.{max}

/** @brief
  *   Concatenate a list of long digits to a single long. Please note: This only
  *   makes sense if the list exclusively contanins single digit nubmers. So for
  *   example List(1,2,3,4,5) becomes 12345.
  *
  * @param list
  *   The elements to concatenate.
  *
  * @return
  *   The joined number.
  */
def numConcatenate(list: Seq[Long]): Long =
    list.foldLeft(0L)((a, b) => 10L * a + b)

@main def main(): Unit =
    val source = scala.io.Source.fromFile("inputs/03.txt")
    val realInput =
        try source.mkString
        finally source.close()
    val dummyInput =
        "987654321111111\n811111111111119\n234234234234278\n818181911112111"
    val input = realInput

    val lines = time("parse"):
        input.split("\n").map(l => l.map(_.toLong - 48).toList).toList

    time("part1 proto"):
        lines
            .map(line =>
                // Compute the highest number that occurs until that index
                //    (inclusive)
                // It has a leading 0 and the last digit is dropped as a pair of
                // numbers can never ever start at that position.
                val highestNumberUntil =
                    line.scanLeft(0L)(max).dropRight(1).drop(1)
                // We now compute the highest number that occurs after the index
                //    (exclusive)
                // This has a trailing 0 (due to reversing) which we do not have
                // to drop since zip handles that for us
                val highestNumberAfter =
                    line.reverse.scanLeft(0L)(max).reverse.drop(1)

                // Pair all the numbers, join them and find the maximum
                highestNumberUntil
                    .zip(highestNumberAfter)
                    .map((l, r) => numConcatenate(List(l, r)))
                    .max
            )
            .sum

    /** @brief
      *   Push a number down the list.
      *
      * If the number is bigger than the first element in the list, replace it.
      * Then the number "freed up" here can be pushed down the rest of the list.
      *
      * 9 => 54321 results in 95432. If the number is not at least as big as the
      * one already in the list, nothing will happen.
      *
      * @param list
      *   The list to push the number down
      * @param newValue
      *   The new value to add into the list.
      *
      * @return
      *   The updated list
      */
    def pushDown(list: List[Long], newValue: Long): List[Long] =
        if list.nonEmpty && newValue >= list(0) then
            pushDown(list.drop(1), list(0)).prepended(newValue)
        else list

    /** @brief
      *   Solve the aoc day 3 problem.
      *
      * We start at the end of the list. We feed those numbers into a list, thus
      * making a new list for the return value candidate.
      *
      * {{{
      * ┌─┬─┬─╦═╦═╗
      * │1│4│6║3║2║
      * └─┴─┴─╩═╩═╝
      *        │ │
      *        ▼ ▼
      *       ┌─┬─┐
      *       │3│2│
      *       └─┴─┘
      * }}}
      *
      * We then continue going through the list. If we encounter a larger number
      * Then we push it into the stack. For that the pushDown function is used.
      *
      * {{{
      *  ┌─┬─┬─╦═╦═╗
      *  │1│4│6║3║2║
      *  └─┴─┴─╩═╩═╝
      *       │  ┌─┬─┐
      *       └─▶│3│2│
      *          └─┴─┘
      *           │ ▲
      *           └─┘
      * }}}
      *
      * The 6 in this example is pushed into the first slot. This frees up the 3
      * which can then be pushed down into the rest of the list, replacing the 2
      * which results in a list of [6,3]. We do this until we processed the list
      * entirely.
      *
      * Passing a new value dpwn into the list has to happen for larger or equal
      * values since the freed up one might still be relevant later in the list.
      * This is all implemented in the pushDown method.
      *
      * @param problemSize
      *   The size of desired output
      * @param list
      *   The input list for this problem
      *
      * @returns
      *   The solution to the probem
      */
    def solveProblemOfSize(problemSize: Int)(list: List[Long]): Long =
        def extend(list: List[Long], newValue: Long): List[Long] =
            // The list is not yet big enough. We can just fill it up for now.
            if list.length < problemSize then list.prepended(newValue)
            // This is where the magic happens.
            // Pushes the new value into the list.
            else pushDown(list, newValue)
        // Fold this onto an empty list. Extend by each element in the list. And
        // finally make it into a number that can be returned.
        numConcatenate(list.reverse.foldLeft(List())(extend))

    time("part1")(lines.map(solveProblemOfSize(2)).sum)
    time("part2")(lines.map(solveProblemOfSize(12)).sum)

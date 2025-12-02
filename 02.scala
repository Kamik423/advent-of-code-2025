import aoc.time
import math.{log10, pow, floor, ceil, max, min}

case class Interval(lower: Long, upper: Long)

def digits(number: Long): Long = log10(number).toLong + 1

@main def aoc25x02(): Unit =
    val source = scala.io.Source.fromFile("inputs/02.txt")
    val input =
        try source.mkString.filterNot(_.isWhitespace)
        finally source.close()

    // val input =
    //     "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

    // // big input test from
    // // https://www.reddit.com/r/adventofcode/comments/1pc9mrg/
    // val input = "1-4294967296"

    val intervals = time("parse"):
        input
            .split(',')
            .map(_.split('-') match {
                case Array(left, right) => Interval(left.toLong, right.toLong)
            })
            .toList

    /** @brief
      *   Find repeating numbers in a certain interval and return them
      *
      * @param interval
      *   The interval in which to search
      * @param amount
      *   The amount or repeats to scan
      */
    def findRepeatsInIntervalNew(interval: Interval, amount: Int): List[Long] =
        // Let's determine the length of the number that can repeat.
        // For interval (1111-1212), amount=2 the length of the repeating segment is 2.
        // We take the total number of digits and divide it by the amount of repeats.
        // The lower bound is rounded up: 2 Repeats in 999--1111 will always be two digits.
        // The upper bound is rounded down.
        //     i.e. if amount is 2 and the upper bound is 111 then there will not be a two
        //     digit repeating number. It has to be one digit in this interval
        val minimumLength = ceil(digits(interval.lower).toFloat / amount).toInt
        val maximumLength = floor(digits(interval.upper).toFloat / amount).toInt
        // We look at all possible lengths. Probably its always gonna be a single number for this input.
        (minimumLength to maximumLength)
            .map(attemptedLength =>
                // The factor is the number to multiply the possible value with.
                // 12 * 101 = 1212     // here 101 is the factor
                // It is generated for any amount of repeats. So it can also be 100100100
                val factor = Range(0, amount)
                    .map(exp => pow(10L, attemptedLength * exp))
                    .sum
                // Lowest possible match is for example 1212/101=12
                // Same for highest possible match. We round again to not get false positives
                // We also clamp it to the right powers of 10.
                //     Otherwise we'd have numbers like 1*101=101 which theoretically is 0101.
                //     However leading zeros are not considered
                val lowestMatch = max(
                  ceil(interval.lower / factor),
                  pow(10L, attemptedLength - 1)
                ).toLong
                val highestMatch = min(
                  floor(interval.upper / factor),
                  pow(10L, attemptedLength) - 1
                ).toLong
                // For the return we multiply it by the factor
                (lowestMatch to highestMatch)
                    .map(_ * factor.toLong)
            )
            .flatten
            .toList

    time("part1"):
        intervals
            .map(interval => findRepeatsInIntervalNew(interval, 2))
            .flatten
            .sum

    time("part2"):
        intervals
            .map(interval =>
                (2 to digits(interval.upper).toInt)
                    .map(amountOfRepeats =>
                        findRepeatsInIntervalNew(interval, amountOfRepeats)
                    )
                    .flatten
            )
            .flatten
            // I need to filter distinct values *here* since 2.2.2.2 == 22.22
            .distinct
            .sum

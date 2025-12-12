package aoc25x05

import aoc.time

case class Interval(lower: Long, upper: Long):
    def contains(value: Long): Boolean =
        lower <= value && value <= upper

    def length: Long =
        upper - lower + 1

@main def main(): Unit =
    val source = scala.io.Source.fromFile("inputs/05.txt")
    val realInput =
        try source.mkString
        finally source.close()
    val dummyInput = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"

    val input = realInput

    val sections = input.trim.split("\n\n", 2)
    assume(sections.length == 2)
    val intervalRegex = """^(\d+)-(\d+)$""".r
    val intervals = time("parse Intervals"):
        sections(0).trim.linesIterator
            .map(intervalRegex.findFirstMatchIn(_))
            .flatten
            .map(m => Interval(m.group(1).toLong, m.group(2).toLong))
            .toList
    val values = time("parse Values"):
        sections(1).trim.linesIterator.map(_.toLong).toList

    time("part1"):
        values.count(value => intervals.exists(_.contains(value)))

    val reducedIntervals = time("part2.1"):
        intervals
            .sortBy(_.lower)
            .foldLeft(List[Interval]())((overlappedIntervals, interval) =>
                if overlappedIntervals.isEmpty then
                    println(interval); List(interval)
                else
                    val lastInterval = overlappedIntervals.last
                    if lastInterval.upper < interval.lower then
                        overlappedIntervals.appended(interval)
                    else if interval.upper <= lastInterval.upper then
                        overlappedIntervals
                    else
                        overlappedIntervals
                            .dropRight(1)
                            .appended(
                              Interval(lastInterval.lower, interval.upper)
                            )
            )

    time("part2.2"):
        reducedIntervals.map(_.length).sum

    time("part1 again"):
        values.count(value => reducedIntervals.exists(_.contains(value)))

    time("part2v2"):
        intervals
            .sortBy(_.lower)
            .foldLeft((0L, Option.empty[Long]))((accumulator, interval) =>
                val (length, upper) = accumulator
                upper match {
                    case None =>
                        (length + interval.length, Some(interval.upper))
                    case Some(upper) =>
                        if upper < interval.lower then
                            (length + interval.length, Some(interval.upper))
                        else if interval.upper <= upper then accumulator
                        else
                            (
                              length + interval.upper - upper,
                              Some(interval.upper)
                            )
                }
            )
            ._1

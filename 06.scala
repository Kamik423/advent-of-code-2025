package aoc25x06

import aoc.time

enum Operation:
    case add, multiply

    def operate(values: Seq[Long]): Long = this match {
        case Operation.add      => values.sum
        case Operation.multiply => values.product
    }

implicit class IndexSplittableString(s: String):
    def splitAt(indices: Seq[Int]): List[String] =
        indices
            .prepended(-1)
            .appended(s.length)
            .sliding(2)
            .map(interval => s.substring(interval(0) + 1, interval(1)))
            .toList

@main def main(): Unit =
    val source = scala.io.Source.fromFile("inputs/06.txt")
    val realInput =
        try source.mkString
        finally source.close()
    val dummyInput =
        "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "

    val input = realInput

    val (numberLines, operatorLine) =
        val lines = input.linesIterator.toList
        (lines.dropRight(1), lines.last)

    val splitIndices = operatorLine.zipWithIndex
        .filterNot(_._1.isWhitespace)
        .map(_._2 - 1)
        .filter(_ > 0)

    val numbersMatrix: List[List[String]] =
        numberLines.map(_.splitAt(splitIndices))
    val operators = operatorLine
        .splitAt(splitIndices)
        .map(_.trim match {
            case "+" => Operation.add
            case "*" => Operation.multiply
            case c   => assert(false, f"Operator '$c' not allowed")
        })

    time("part1"):
        val numbers = numbersMatrix
            .map(_.map(_.trim.toLong))
            .transpose
        numbers
            .zip(operators)
            .map((values, operation) => operation.operate(values))
            .sum

    time("part2"):
        val numbers =
            numbersMatrix.transpose.map(_.transpose.map(_.mkString.trim.toLong))
        numbers
            .zip(operators)
            .map((values, operation) => operation.operate(values))
            .sum

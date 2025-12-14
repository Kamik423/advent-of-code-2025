package aoc25x07

import aoc.time

enum Element:
    case empty, start, split

@main def main(): Unit =
    val source = scala.io.Source.fromFile("inputs/07.txt")
    val realInput =
        try source.mkString
        finally source.close()
    val dummyInput =
        ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n..............."

    val input = realInput

    val lines: List[List[Element]] = time("parse"):
        input.linesIterator
            .map(_.map(_ match {
                case '.' => Element.empty
                case 'S' => Element.start
                case '^' => Element.split
            }).toList)
            .toList

    val lastRow = time("compute"):
        lines.foldLeft(List.fill(lines(0).length)((0L, 0L)))({ (beams, line) =>
            beams
                .zip(line)
                .map((data, element) =>
                    val (beams, splitCount) = data
                    element match {
                        case Element.empty => ((0L, beams, 0L), splitCount)
                        case Element.start => ((0L, beams + 1L, 0L), splitCount)
                        case Element.split =>
                            ((beams, 0L, beams), splitCount + beams.sign)
                    }
                )
                .prepended(((0L, 0L, 0L), 0L))
                .appended(((0L, 0L, 0L), 0L))
                .sliding(3)
                .map(data =>
                    val (left, middle, right) = (data(0), data(1), data(2))
                    (left(0)(2) + middle(0)(1) + right(0)(0), middle(1))
                )
                .toList
        })

    time("part1"):
        lastRow.map(_._2).sum

    time("part2"):
        lastRow.map(_._1).sum

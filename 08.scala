package aoc25x08

import aoc.time
import scala.collection.immutable.{Iterable, HashMap}
import scala.math.sqrt

case class Node(x: Long, y: Long, z: Long):
    def squareistanceTo(other: Node): Long =
        val dx = (x - other.x)
        val dy = (y - other.y)
        val dz = (z - other.z)
        dx * dx + dy * dy + dz * dz

type NodeGroups = Map[Node, Int]
type NodePair = (Node, Node)

implicit class ConnectableNodeGroups(nodeGroups: NodeGroups):
    def connectingGroups(group1: Int, group2: Int): NodeGroups =
        val newGroup = group1.min(group2)
        val oldGroup = group1.max(group2)
        nodeGroups.transform((node, group) =>
            if group == oldGroup then newGroup else group
        )

implicit class InvertableMap[K, V](map: Map[K, V]):
    def groupByKey: Map[V, List[K]] =
        map.groupMap((key: K, value: V) => value)((key: K, value: V) => key)
            .transform((key: V, value: Iterable[K]) => value.toList)

@main def main(): Unit =
    val source = scala.io.Source.fromFile("inputs/08.txt")
    val realInput =
        try source.mkString
        finally source.close()
    val dummyInput =
        "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"

    val input = realInput

    val partOneConnectionCount = if input == dummyInput then 10 else 1000

    val nodes: List[Node] = time("parse"):
        input.linesIterator
            .map(_.split(",").map(_.toLong))
            .map(coordinates =>
                require(coordinates.length == 3)
                Node(coordinates(0), coordinates(1), coordinates(2))
            )
            .toList

    val distanceSortedNodePairs: List[NodePair] = time("distances"):
        nodes
            .combinations(2)
            .toList
            .map(l => (l(0), l(1)))
            .sortBy(_.squareistanceTo(_))

    val initialGroups: NodeGroups = HashMap.from(nodes.zipWithIndex)

    time("part1"):
        val remainingGroups = distanceSortedNodePairs
            .take(partOneConnectionCount)
            .foldLeft(initialGroups)((nodeGroups: NodeGroups, pair: NodePair) =>
                val (g1, g2) = (nodeGroups(pair(0)), nodeGroups(pair(1)))
                if g1 == g2 then nodeGroups
                else nodeGroups.connectingGroups(g1, g2)
            )
            .groupByKey
            .transform((group, nodes) => nodes.size)
        remainingGroups.values.toList.sorted.takeRight(3).product

    type CarryOver = (NodeGroups, Option[(Long)])

    time("part2"):
        distanceSortedNodePairs
            .foldLeft[CarryOver]((initialGroups, Option.empty[(Long)]))(
              (originalData: CarryOver, pair: NodePair) =>
                  val (nodeGroups: NodeGroups, _) = originalData
                  val (g1, g2) = (nodeGroups(pair(0)), nodeGroups(pair(1)))
                  if g1 == g2 then originalData
                  else
                      (
                        nodeGroups.connectingGroups(g1, g2),
                        Some(pair(0).x * pair(1).x)
                      )
            )
            ._2
            .get

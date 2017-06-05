package wust.bench

// http://chariotsolutions.com/blog/post/microbenchmarking-scala-with-jmh/

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit

import java.util.concurrent.TimeUnit

object Data {
  object dfs {
    val start = 1
    val next = Map(
      1 -> List(2, 3),
      2 -> List(1),
      3 -> List(4, 5, 6),
      4 -> List(6),
      5 -> List(7),
      6 -> Nil,
      7 -> Nil
    )
  }
}
/* Default settings for benchmarks in this class */
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
class Graph {

  @Benchmark
  def depthFirstIterator: Unit = wust.util.algorithm.depthFirstSearchIterator(Data.dfs.start, Data.dfs.next).size
}

package sparsity.common.parser

import fastparse.Parsed

object ErrorMessage {

  def indexToLine(lines: Seq[String], index: Int): (Int, Int) = {
    val cumulativeLengths = lines.scanLeft(0)(_ + _.length)
    cumulativeLengths.zipWithIndex
      .sliding(2)
      .collectFirst {
        case Seq((prev, _), (curr, i)) if index < curr =>
          (i - 1, index - prev)
      }
      .getOrElse((lines.length - 1, 0))
  }

  def format(query: String, msg: Parsed.Failure): String = {
    val lines = query.split("\n")

    // println(msg.longMsg)

    val expected: String = "expected " + msg.label
    val index: Int = msg.extra.index
    // if(msg.extra.stack.isEmpty){ ("parse error", ) }
    // else {
    //   val (expected, index) = msg.extra.stack(0)
    //   (" expected "+expected, index)
    // }
    val (lineNumber, linePosition) = indexToLine(lines, index)
    return lines(lineNumber) + "\n" +
      " " * linePosition +
      "^--- " + expected
  }

}

package sparsity.parser

import scala.collection.mutable.Buffer
import scala.io._
import java.io._
import fastparse._, NoWhitespace._
import com.typesafe.scalalogging.slf4j.LazyLogging

class StreamParser[R](parser:(Iterator[String] => Parsed[R]), source: Reader)
  extends Iterator[Parsed[R]]
  with LazyLogging
{
  val bufferedSource = source match { 
    case b:BufferedReader => b
    case _ => new BufferedReader(source)
  }
  val buffer = Buffer[String]()

  def load
  {
    while(bufferedSource.ready){
      buffer += bufferedSource.readLine.replace("\\n", " ");
    }
  }

  def loadBlocking
  {
    buffer += bufferedSource.readLine.replace("\\n", " ");
  }

  def hasNext(): Boolean = { load; buffer.size > 0 }
  def next:Parsed[R] =
  {
    load
    if(buffer.size > 0) {
      parser(buffer.iterator) match {
        case r@Parsed.Success(result, index) => 
          logger.debug(s"Parsed(index = $index): $result")
          skipBytes(index)
          return r
        case f:Parsed.Failure => 
          buffer.clear()
          return f
      }
    } else {
      throw new IndexOutOfBoundsException("reading from an empty iterator")
    }
  }

  def skipBytes(offset: Int)
  {
    var dropped = 0
    while(offset > dropped && !buffer.isEmpty){
      logger.trace(s"Checking for drop: $dropped / $offset: Next unit: ${buffer.head.length}")
      if(buffer.head.length < (offset - dropped)){ 
        dropped = dropped + buffer.head.length
        logger.trace(s"Dropping '${buffer.head}' ($dropped / $offset dropped so far)")
        buffer.remove(0)
      } else {
        logger.trace(s"Trimming '${buffer.head}' (by ${offset - dropped})")
        var head = buffer.head
                         .substring(offset - dropped)
                         .replace("^\\w+", "")
        logger.trace(s"Trimming leading whitespace")
        while(head.length <= 0 && !buffer.isEmpty){
          logger.trace(s"Nothing but whitespace left.  Dropping and trying again")
          buffer.remove(0)
          if(!buffer.isEmpty) {
            head = buffer.head.replace("^\\w+", "")
          }
        }
        logger.trace(s"Remaining = '$head'")
        if(head.length > 0){ buffer.update(0, head) }
        return
      }
    }
  }

}

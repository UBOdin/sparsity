package sparsity

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

  def hasNext(): Boolean = { load; buffer.size > 0 }
  def next:Parsed[R] =
  {
    load
    if(buffer.size > 0) {
      parser(buffer.iterator) match {
        case r@Parsed.Success(result, index) => 
          logger.info(s"Parsed(index = $index): $result")
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
      logger.debug(s"Checking for drop: $dropped / $offset: Next unit: ${buffer.head.length}")
      if(buffer.head.length < (offset - dropped)){ 
        dropped = dropped + buffer.head.length
        logger.debug(s"Dropping '${buffer.head}' ($dropped / $offset dropped so far)")
        buffer.remove(0)
      } else {
        logger.debug(s"Trimming '${buffer.head}' (by ${offset - dropped}) and done")
        buffer.update(0, buffer.head.substring(offset - dropped))
        return
      }
    }
  }

}

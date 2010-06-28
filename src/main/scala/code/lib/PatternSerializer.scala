package code {
package lib {

import net.liftweb.json.{Formats, MappingException, Serializer, TypeInfo}
import net.liftweb.json.JsonAST._

import java.util.regex.{Pattern, PatternSyntaxException}

/*
* Provides a way to serialize/de-serialze Patterns.
*
* Queries for a Pattern (pattern) using the lift-json DSL would look like:
* ("pattern" -> ("$pattern" -> pattern.pattern))
* ("pattern" -> ("$pattern" -> "^Mo"))
*/
class PatternSerializer extends Serializer[Pattern] {
  private val PatternClass = classOf[Pattern]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Pattern] = {
    case (TypeInfo(PatternClass, _), json) => json match {
      case JObject(JField("$pattern", JString(s)) :: Nil) =>
        try {
          println("deserializing pattern: "+s) // this never gets called
          Pattern.compile(s)
        }
        catch {
          case e: PatternSyntaxException => throw new MappingException("Can't compile " + s + " to Pattern")
        }
      case x => throw new MappingException("Can't convert " + x + " to Pattern")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: Pattern => JObject(JField("$pattern", JString(x.pattern)) :: Nil)
  }
}

class IntervalSerializer extends Serializer[Interval] {
  private val IntervalClass = classOf[Interval]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Interval] = {
    case (TypeInfo(IntervalClass, _), json) => json match {
      case JObject(JField("start", JInt(s)) :: JField("end", JInt(e)) :: Nil) =>
        new Interval(s.longValue, e.longValue)
      case x => throw new MappingException("Can't convert " + x + " to Interval")
    }
  }

  def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
    case x: Interval =>
      JObject(JField("start", JInt(BigInt(x.startTime))) :: 
              JField("end",   JInt(BigInt(x.endTime))) :: Nil)
  }
}

class Interval(start: Long, end: Long) {
  val startTime = start
  val endTime = end
}

}
}
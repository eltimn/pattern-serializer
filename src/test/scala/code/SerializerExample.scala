package code {

import lib._

import java.util.Date
import java.util.regex.Pattern

import org.specs.Specification

import net.liftweb.json.{NoTypeHints, Serialization}
import net.liftweb.json.Serialization.{read, write => swrite}
import net.liftweb.json.JsonAST._

object SerializerExample extends Specification {

  "Date Serializer Example" in {

    implicit val formats = Serialization.formats(NoTypeHints) + new DateSerializer

    /*
    * Date serialization
    */
    val d = new Date
    var dser = swrite(d)
    dser must_== "{\"$dt\":\""+formats.dateFormat.format(d)+"\"}"

    val d2 = read[Date](dser) // FIXME: throws net.liftweb.json.MappingException:
    // Do not know how to convert JObject(List(JField($dt,JString(2010-06-28T10:09:20.255Z)))) into class java.util.Date
    d2 must_== d
  }

  "Pattern Serializer Example" in {

    implicit val formats = Serialization.formats(NoTypeHints) + new IntervalSerializer + new PatternSerializer

    /*
    * Interval serialization
    */
    val i = new Interval(1, 4)
    val ser = swrite(i)
    ser mustEqual """{"start":1,"end":4}"""

    val i2 = read[Interval](ser)
    i2.startTime mustEqual i.startTime
    i2.endTime mustEqual i.endTime

    // simple Pattern
    val p = Pattern.compile("^Curly")

    val pser = swrite(p)
    pser must_== """{"$pattern":"^Curly"}"""

    val p2 = read[Pattern](pser) // FIXME: throws net.liftweb.json.MappingException: unknown error
    p2 must_== p

    // Pattern in a case class
    case class PatternExample(id: String, regx: Pattern)
    val pe = PatternExample("1", Pattern.compile("^Moe"))

    val peser = swrite(pe)
    peser must_== """{"id":"1","regx":{"$pattern":"^Moe"}}"""

    val pe2 = read[PatternExample](peser) // FIXME: throws net.liftweb.json.MappingException: unknown error
    pe2.id must_== pe.id
    pe2.regx must_== pe.regx
  }
}


}

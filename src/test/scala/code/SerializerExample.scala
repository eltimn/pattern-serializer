package code {

import lib.{Interval, IntervalSerializer, PatternSerializer}

import java.util.regex.Pattern

import org.specs.Specification
  
object SerializerExample extends Specification {
  "Pattern Serializer Example" in {
  
    import net.liftweb.json.{NoTypeHints, Serialization}
    import net.liftweb.json.Serialization.{read, write => swrite}
    import net.liftweb.json.JsonAST._
    
    implicit val formats = Serialization.formats(NoTypeHints) + new PatternSerializer + new IntervalSerializer
      
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
package darts.lib.json.read

sealed trait Value 

object Value {
    
    final case object Null extends Value
    
    final case class Bool (val value: Boolean) extends Value
    final case class Str(val value: String) extends Value
    final case class Num(val value: BigDecimal) extends Value
    final case class List(val value: Seq[Value]) extends Value
    final case class Dict(val value: Map[String,Value]) extends Value

    val True = Bool(true)
    val False = Bool(false)

    abstract class Scalaish[T] private[Value] () {
        
        protected def tryFromString(s: String): Option[T] = None
        protected def tryFromNumber(b: BigDecimal): Option[T] = None
        protected def tryFromBoolean(b: Boolean): Option[T] = None

        def unapply(v: Value): Option[T] = v match {
            case Str(s) => tryFromString(s)
            case Num(s) => tryFromNumber(s)
            case Bool(s) => tryFromBoolean(s)
            case _ => None
        }
        
        lazy val OrNull = new NullOrScalaish[T](this)
    }
    
    final class NullOrScalaish[T] private[Value] (private val inner: Scalaish[T]) {
        def unapply(v: Value): Option[Option[T]] = v match {
            case Null => Some(None)
            case oth => inner.unapply(oth) match {
                case None => None
                case s@Some(_) => Some(s)
            }
        }
    }
    
    abstract class AbstractNumberish[T] private[Value] () extends Scalaish[T] {
        
        protected def extractFromBigDecimal(bd: BigDecimal): T
        protected def extractFromString(bd: String): T
        
        override protected def tryFromBoolean(b: Boolean): Option[T] = None
        
        override protected def tryFromNumber(b: BigDecimal): Option[T] = {
            try Some(extractFromBigDecimal(b))
            catch {
                case _: ArithmeticException => None
            }
        }
        
        override protected def tryFromString(s: String): Option[T] = {
            try Some(extractFromString(s))
            catch {
                case _: NumberFormatException => None
            }
        }
    }
    
    final object Longish extends AbstractNumberish[Long] {
        protected def extractFromBigDecimal(bd: BigDecimal): Long = bd.toLongExact
        protected def extractFromString(bd: String): Long = bd.toLong
    }
    
    final object Integerish extends AbstractNumberish[Int] {
        protected def extractFromBigDecimal(bd: BigDecimal): Int = bd.toIntExact
        protected def extractFromString(bd: String): Int = bd.toInt
    }
    
    final object Shortish extends AbstractNumberish[Short] {
        protected def extractFromBigDecimal(bd: BigDecimal): Short = bd.toShortExact
        protected def extractFromString(bd: String): Short = bd.toShort
    }
    
    final object Byteish extends AbstractNumberish[Byte] {
        protected def extractFromBigDecimal(bd: BigDecimal): Byte = bd.toByteExact
        protected def extractFromString(bd: String): Byte = bd.toByte
    }
    
    final object BigDecimalish extends AbstractNumberish[BigDecimal] {
        protected def extractFromBigDecimal(bd: BigDecimal): BigDecimal = bd
        protected def extractFromString(bd: String): BigDecimal = BigDecimal(bd)
    }
    
    final object Charish extends Scalaish[Char] {
    	override protected def tryFromString(s: String): Option[Char] = 
    	    if (s.length == 1) Some(s.charAt(0)) else None
    }
    
    final object Booleanish extends Scalaish[Boolean] {
        private val z = BigDecimal(0)
        override protected def tryFromNumber(b: BigDecimal): Option[Boolean] = Some(b != z)
        override protected def tryFromBoolean(b: Boolean): Option[Boolean] = Some(b)
        override protected def tryFromString(s: String): Option[Boolean] = s match {
            case "true" | "yes" | "1" => Some(true)
            case "false" | "no" | "0" => Some(false)
            case _ => None
        }
    }
    
    final object Stringish extends Scalaish[String] {
        override protected def tryFromNumber(b: BigDecimal): Option[String] = Some(b.toString)
        override protected def tryFromBoolean(b: Boolean): Option[String] = Some(b.toString)
        override protected def tryFromString(s: String): Option[String] = Some(s)
    }
}

sealed trait Event {

    def location: Location
}

object Event {
    
    final case class Literal(val location: Location, val value: Value, val key: Option[String]) extends Event
    final case class ListStart(val location: Location, val key: Option[String]) extends Event
    final case class ListEnd(val location: Location, val key: Option[String]) extends Event
    final case class MappingStart(val location: Location, val key: Option[String]) extends Event
    final case class MappingEnd(val location: Location, val key: Option[String]) extends Event
    
    final object Keyed {
        def unapply(stream: Stream[Event]): Option[(String,Value,Location, Stream[Event])] = stream match {
            case Literal(location, value, Some(key)) #:: tail => Some((key, value, location, tail))
            case _ => None
        }
    }
}
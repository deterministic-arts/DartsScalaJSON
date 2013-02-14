package darts.lib.json.write

import java.io.Writer
import java.util.Locale

import org.joda.time.{DateTime, Instant}
import org.joda.time.chrono.ISOChronology
import org.joda.time.format.DateTimeFormat

trait Formatter[T] {
    
    /**
     * Type of the values being handled by this formatter
     */
	
    type Rep = T

    /**
     * Generate a JSON representation of `value`, writing it
     * using `renderer`. The implementation may either write
     * the data directly into the renderer's underlying output
     * stream using [[Renderer#write]] or use the renderer's
     * higher level methods for generating the desired 
     * representation.
     * 
     * @param renderer	renderer to write into
     * @param value		value to write
     */
    
    def format(renderer: Renderer, value: Rep): Unit
}

object Formatter {
    
    private val DateTimeFmt = 
        DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
        			  .withChronology(ISOChronology.getInstanceUTC())
        			  .withLocale(Locale.ROOT)
    
    /**
     * Writes `value` into `writer`'s underlying output stream,
     * properly encoded as JSON string value. This method encodes
     * all characters, which are not printable ASCII using unicode
     * character escapes. Other special characters are escaped 
     * using their canonical escape sequence. The string is 
     * enclosed in double-quote characters (ASCII 34). 
     * 
     * @param renderer	renderer to write into
     * @param value		string to write
     */
        			  
    def writeString(writer: Renderer, value: String): Unit = {
        		
		val length = value.length()
		var start: Int = 0
		var p: Int = 0
        
        def writeStr(str: String) {
            writer.write(str)
        }
        
        def writeSeq(start: Int, end: Int): Unit = {
            writer.write(value, start, end - start)
        }
        
        def writeRep(str: String) {
            if (start < p) writeSeq(start, p)
            writeStr(str)
            p += 1
            start = p
        }
        
        writeStr("\"");
		
		while (p < length) {
			
			val ch = value.charAt(p);
			
			ch match  {
				case '\0' => writeRep("\\0")
				case '\n' => writeRep("\\n")
				case '\r' => writeRep("\\r")
				case '\t' => writeRep("\\t")
				case '\"' => writeRep("\\\"")
				case cc if 32 <= cc && cc <= 126 => { p += 1 }
				case _ => {
				    if (start < p) writeSeq(start, p)
				    writeStr("\\u%04x".format(ch.asInstanceOf[Int]))
				    p += 1
				    start = p
				}
			}
		}

		if (start < p) writeSeq(start, p) 
		writeStr("\"")
    }
    
    implicit object ByteFormatter extends Formatter[Byte] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(value.toString)
    }
    
    implicit object ShortFormatter extends Formatter[Short] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(value.toString)
    }
    
    implicit object IntFormatter extends Formatter[Int] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(value.toString)
    }
    
    implicit object LongFormatter extends Formatter[Long] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(value.toString)
    }
    
    implicit object FloatFormatter extends Formatter[Float] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(value.toString)
    }
    
    implicit object DoubleFormatter extends Formatter[Double] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(value.toString)
    }
    
    implicit object BigDecimalFormatter extends Formatter[BigDecimal] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(value.toString)
    }
    
    implicit object BooleanFormatter extends Formatter[Boolean] {
        def format(writer: Renderer, value: Rep): Unit = writer.write(if (value) "true" else "false")
    }
    
    implicit object CharFormatter extends Formatter[Char] {
        def format(writer: Renderer, value: Rep): Unit = StringFormatter.format(writer, "" + value)
    }
    
    implicit object StringFormatter extends Formatter[String] {
        def format(writer: Renderer, value: Rep): Unit = writeString(writer, value)
    }
    
    implicit object DateTimeFormatter extends Formatter[DateTime] {
        def format(writer: Renderer, value: Rep): Unit = writeString(writer, value.toString(DateTimeFmt))
    }
    
    implicit object InstantFormatter extends Formatter[Instant] {
        def format(writer: Renderer, value: Rep): Unit = writeString(writer, value.toString(DateTimeFmt))
    }
    
    implicit object NullFormatter extends Formatter[Null] {
        def format(writer: Renderer, value: Rep): Unit = writer.write("null")
    }
    
    implicit object SymbolFormatter extends Formatter[Symbol] {
    	def format(writer: Renderer, value: Rep): Unit = writeString(writer, value.name)   
    }
    
    final class OptionFormatter[T] (val inner: Formatter[T]) extends Formatter[Option[T]] {
        def format(writer: Renderer, value: Rep): Unit = writer.sub { act =>
    		if (value.isEmpty) act.undefined 
    		else act.value(value.get)(inner)
        }
    }
    
    implicit object URIFormatter extends Formatter[java.net.URI] {
        def format(writer: Renderer, value: Rep): Unit = writeString(writer, value.toASCIIString)
    }
    
    /**
     * Returns a formatter for an option type, provided, that there
     * is a formatter for the option's enclosed value type. `None`
     * is rendered as `null`, and `Some(x)` is rendered using the
     * proper representation of `x` according to `fmt`
     * 
     * @param fmt	formatter for the option's enclosed type
     */
    
    implicit def optionFormatter[T](implicit fmt: Formatter[T]): Formatter[Option[T]] = 
        new OptionFormatter[T](fmt)
}
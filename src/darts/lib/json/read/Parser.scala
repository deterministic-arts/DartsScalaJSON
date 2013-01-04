package darts.lib.json.read

class ParserException (val location: Location, message: String) 
extends java.io.IOException("%s: %s".format(location, message))

object Parser {
    
    private def trailingGarbage(token: Token) = 
		throw new ParserException(token.location, "trailing garbage after value: %s".format(token))

    private def prematureEof(msg: String) = 
        throw new ParserException(Location.Unknown, "unexpected end of input; " + msg)
    
    private def unexpectedToken(token: Token, msg: String) = 
        throw new ParserException(token.location, "unexpected token %s: %s".format(token, msg))
            
    private def toBool(value: Boolean): Value = 
        if (value) Value.True else Value.False
    
    private def advance(s: Stream[Token]): Stream[Token] = {
        if (s.isEmpty) s else s.head match {
            case Token.EndOfInput(_) => Stream.empty
            case Token.SyntaxError(code, message, location) => throw new ParserException(location, message + " (" + code + ")")
            case _ => s
        }
    }

    def parse(s: Stream[Token]): Stream[Event] = {
        parse(s, st => advance(st) match {
            case tok #:: tail => trailingGarbage(tok)
            case _ => Stream.empty
        })
    }
    
    def parse(s: Stream[Token], cont: Stream[Token]=>Stream[Event]): Stream[Event] = {
        
        def produce(evt: Event, s: Stream[Token])(k: Stream[Token]=>Stream[Event]): Stream[Event] = 
            Stream.cons(evt, k(s))
            
        def parseArray(s: Stream[Token], key: Option[String])(k: Stream[Token]=>Stream[Event]): Stream[Event] = {
            advance(s) match {
                case Token.RightBracket(loc) #:: tail => produce(Event.ListEnd(loc, key), tail)(k)
                case n@(_ #:: tail) => parseValue(n, None, false) { s => 
                	def parseComma(s: Stream[Token]): Stream[Event] = advance(s) match {
                	    case Token.RightBracket(loc) #:: tail => produce(Event.ListEnd(loc, key), tail)(k)
                	    case Token.Comma(loc) #:: tail => parseValue(tail, None, false)(parseComma)
                	    case tk #:: tail => unexpectedToken(tk, "',' or ']' expected")
                	    case _ => prematureEof("',' or ']' expected")
                	}
                	parseComma(s)
                }
                case _ => prematureEof("value form or ']' expected")
            }
        }
        
        def parseEntry(s: Stream[Token])(k: Stream[Token]=>Stream[Event]): Stream[Event] = advance(s) match {
            case Token.StringLiteral(value, loc) #:: tail => advance(tail) match {
                case Token.Colon(loc) #:: tail => parseValue(tail, Some(value), false)(k)
                case tk #:: tail => unexpectedToken(tk, "':' expected")
                case _ => prematureEof("':' expected")
            }
            case tk #:: tail => unexpectedToken(tk, "string literal expected")
            case _ => prematureEof("string literal expected")
        }
        
        def parseMapping(s: Stream[Token], key: Option[String])(k: Stream[Token]=>Stream[Event]): Stream[Event] = {
            advance(s) match {
                case Token.RightCurly(loc) #:: tail => produce(Event.MappingEnd(loc, key), tail)(k)
                case n@(_ #:: tail) => parseEntry(n) { s => 
                    def parseComma(s: Stream[Token]): Stream[Event] = advance(s) match {
                        case Token.RightCurly(loc) #:: tail => produce(Event.MappingEnd(loc, key), tail)(k)
                        case Token.Comma(loc) #:: tail => parseEntry(tail)(parseComma)
                        case tk #:: tail => unexpectedToken(tk, "',' or '}' expected")
                        case _ => prematureEof("',' or '}' expected")
                    }
                    parseComma(s)
                }
                case _ => prematureEof("value form or '}' expected")
            }
        }
        
        def arrayParser(key: Option[String], k: Stream[Token]=>Stream[Event]): Stream[Token]=>Stream[Event] = 
            (s: Stream[Token]) => parseArray(s, key)(k)
        
        def mappingParser(key: Option[String], k: Stream[Token]=>Stream[Event]): Stream[Token]=>Stream[Event] = 
            (s: Stream[Token]) => parseMapping(s, key)(k)
            
        def parseValue(s: Stream[Token], key: Option[String], top: Boolean)(k: Stream[Token]=>Stream[Event]): Stream[Event] = {
            advance(s) match {
                case Token.BooleanLiteral(value, loc) #:: tail =>  produce(Event.Literal(loc, toBool(value), key), tail)(k)
                case Token.NullLiteral(loc) #:: tail => produce(Event.Literal(loc, Value.Null, key), tail)(k)
                case Token.StringLiteral(value, loc) #:: tail => produce(Event.Literal(loc, Value.Str(value), key), tail)(k)
                case Token.NumberLiteral(value, loc) #:: tail => produce(Event.Literal(loc, Value.Num(value), key), tail)(k)
                case Token.LeftBracket(loc) #:: tail => produce(Event.ListStart(loc, key), tail)(arrayParser(key, k))
                case Token.LeftCurly(loc) #:: tail => produce(Event.MappingStart(loc, key), tail)(mappingParser(key, k))
                case tk #:: tail => unexpectedToken(tk, "value form expected")
                case _ => if (top) Stream.empty else prematureEof("value form expected")
            }
        } 
        
        parseValue(s, None, true)(cont)
    }
    
    def skipValue(s: Stream[Event]): Stream[Event] = {
        
        import scala.annotation.tailrec
        
        def skipMapping(s: Stream[Event]): Stream[Event] = 
            skipMappingLoop(s)
            
        def skipList(s: Stream[Event]): Stream[Event] = 
            skipListLoop(s)
        
        @tailrec def skipMappingLoop(s: Stream[Event]): Stream[Event] = s match {
            case Event.MappingEnd(_, _) #:: tail => tail
            case Event.Literal(_, _, _) #:: tail => skipMappingLoop(tail)
            case Event.ListStart(_, _) #:: tail => skipMappingLoop(skipList(tail))
            case Event.MappingStart(_, _) #:: tail => skipMappingLoop(skipMapping(tail))
            case evt => throw new AssertionError("" + evt)
        }
        
        @tailrec def skipListLoop(s: Stream[Event]): Stream[Event] = s match {
            case Event.ListEnd(_, _) #:: tail => tail
            case Event.Literal(_, _, _) #:: tail => skipListLoop(tail)
            case Event.MappingStart(_, _) #:: tail => skipListLoop(skipMapping(tail))
            case Event.ListStart(_, _) #:: tail => skipListLoop(skipList(tail))
            case evt => throw new AssertionError("" + evt) 
        }
        
        s match {
            case Event.Literal(_, _, _) #:: tail => tail
            case Event.ListStart(_, _) #:: tail => skipList(tail)
            case Event.MappingStart(_, _) #:: tail => skipMapping(tail)
            case evt #:: tail => throw new ParserException(evt.location, "unexpected event " + evt)
            case _ => Stream.empty
        }
    }
    
    def parseValue(data: String): Option[Value] =
        parseValue(data, 0, data.length)
    
    def parseValue(data: String, start: Int): Option[Value] =
        parseValue(data, start, data.length)
    
    def parseValue(data: String, start: Int, end: Int): Option[Value] = { 
        val (answer, rest) = parseValue(parse(new StringTokenizer(data, start, end).toStream)) 
        assert(rest.isEmpty)
        answer
	}	
    
    def parseValue(s: Stream[Event]): (Option[Value], Stream[Event]) = {
        
        import scala.annotation.tailrec
        import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}
        
        def parseMapping(s: Stream[Event]): (Value, Stream[Event]) = 
            parseMappingLoop(s, Map())
            
        def parseList(s: Stream[Event]): (Value, Stream[Event]) = 
            parseListLoop(s, new ArrayBuffer[Value])
        
        @tailrec def parseMappingLoop(s: Stream[Event], buf: Map[String,Value]): (Value, Stream[Event]) = s match {
            case Event.MappingEnd(_, _) #:: tail => (Value.Dict(buf), tail)
            case Event.Literal(_, value, Some(key)) #:: tail => parseMappingLoop(tail, buf + (key -> value))
            case Event.ListStart(_, Some(key)) #:: tail => {
                val (list, continuation) = parseList(tail)
                parseMappingLoop(continuation, buf + (key -> list))
            }
            case Event.MappingStart(_, Some(key)) #:: tail => {
                val (mapping, continuation) = parseMapping(tail)
                parseMappingLoop(continuation, buf + (key -> mapping))
            }
            case _ => throw new AssertionError
        }
        
        @tailrec def parseListLoop(s: Stream[Event], buf: ArrayBuffer[Value]): (Value, Stream[Event]) = s match {
            case Event.Literal(_, value, None) #:: tail => { buf += value; parseListLoop(tail, buf) }
            case Event.ListEnd(_, None) #:: tail => (Value.List(buf.toSeq), tail)
            case Event.MappingStart(_, None) #:: tail => { val (map, next) = parseMapping(tail); buf += map; parseListLoop(next, buf) }
            case Event.ListStart(_, None) #:: tail => { val (list, next) = parseList(tail); buf += list; parseListLoop(next, buf) }
            case _ => throw new AssertionError
        }
        
        s match {
            case Event.Literal(_, value, _) #:: tail => (Some(value), tail)
            case Event.ListStart(_, _) #:: tail => { val (a,b) = parseList(tail); (Some(a), b) }
            case Event.MappingStart(_, _) #:: tail => { val (a,b) = parseMapping(tail); (Some(a), b) }
            case evt #:: tail => throw new ParserException(evt.location, "unexpected event " + evt)
            case _ => (None, Stream.empty)
        }
    }
}

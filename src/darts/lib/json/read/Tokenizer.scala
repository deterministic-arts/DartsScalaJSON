package darts.lib.json.read

import scala.annotation.tailrec
import java.io.{Reader, StringReader}

object Tokenizer {
    private val InitialToken = Token.SyntaxError(SyntaxErrorCode.IllegalLexerState, "", Location.Unknown)
}

abstract class Tokenizer {
	
    import Tokenizer._
    
    protected def source: String
    protected def nextChar: Int

    private var _buffer: Int = -1
    private var _offset: Int = 0
    private var _line: Int = 1
    private var _token: Token = InitialToken
    private var _done: Boolean = false

    def get: Token = _token
    
    private def peek: Int = {
        if (_buffer >= 0) _buffer
        else {
            _buffer = nextChar
            _buffer
        }
    }
    
    private def read: Int = {
        val ch = (if (_buffer < 0) nextChar else {
            val answer = _buffer
            _buffer = -1
            answer
        })
        if (ch < 0) -1 else {
            _offset += 1
            ch match {
                case '\n' => { _line += 1; ch }
                case _ => ch
            }
        }
    }
    
    def next: Boolean = if (_done) false else {
        
        def provide(tok: Token): Boolean = {
            _token = tok
            true
        }
        
        def eofAt(offset: Int, line: Int): Boolean = {
            _token = Token.EndOfInput(Location(source, offset, line))
            _done = true
            false
        }
        
        def errorAt(offset: Int, line: Int, code: SyntaxErrorCode.Value, fmt: String, args: Any*): Boolean = {
            _token = Token.SyntaxError(code, fmt.format(args), Location(source, offset, line))
            _done = true
            false
        }
        
        def badCharAt(offset: Int, line: Int): Boolean = {
            errorAt(offset, line, SyntaxErrorCode.IllegalCharacter, "invalid character")
        }
        
        def readString: Boolean = {
            
            val offset = _offset
            val line = _line
            val buffer = new StringBuilder
            
            def addc(ch: Int) = buffer += ch.asInstanceOf[Char]
            def adds(str: String) = str.foreach(c=>addc(c))
            
            def readhex(left: Int, accu: Int): Boolean = {
                if (left == 0) { addc(accu); true } else {
                    val ch = read
                    if (ch < 0) errorAt(offset, line, SyntaxErrorCode.UnterminatedStringLiteral, "unterminated literal")
                    else if ('0' <= ch && ch <= '9') readhex(left - 1, (ch - '0') | (accu << 4))
                    else if ('a' <= ch && ch <= 'f') readhex(left - 1, (ch - 'a' + 10) | (accu << 4))
                    else if ('A' <= ch && ch <= 'F') readhex(left - 1, (ch - 'A' + 10) | (accu << 4))
                    else errorAt(offset, line, SyntaxErrorCode.IllegalBackslashEscape, "invalid backslash escape sequence")
                }
            }
            
            def escaped(ch: Int): Boolean = ch match {
                case '\\' => { addc('\\'); true }
                case '"' => { addc('"'); true }
                case 'b' => { addc('\b'); true }
                case 'r' => { addc('\r'); true }
                case 'n' => { addc('\n'); true }
                case 't' => { addc('\t'); true }
                case '/' => { addc('/'); true }
                case 'u' => { readhex(4, 0) }
                case k if k < 0 => errorAt(offset, line, SyntaxErrorCode.UnterminatedStringLiteral, "unterminated literal")
                case _ => badCharAt(offset, line)
            }
            
            @tailrec def mainloop(ch: Int): Boolean = ch match {
                case '"' => provide(Token.StringLiteral(buffer.result(), Location(source, offset, line)))
                case '\\' => { escaped(read) && mainloop(read) }
                case k if k < 32 => errorAt(offset, line, SyntaxErrorCode.UnterminatedStringLiteral, "unterminated literal")
                case k => { addc(k); mainloop(read) }
            }
            
            mainloop(read)
        }
        
        def readNumber: Boolean = {
            
            val BeforeSign = 0
            val AfterSign = 1
            val Integer = 2
            val AfterDot = 3
            val Fraction = 4
            val AfterExpMarker = 5
            val AfterExpSign = 6
            val Exponent = 7
            
            val offset = _offset
            val line = _line
            val buffer = new StringBuilder
            
            def done(md: Int): Boolean = md match {
                case Integer | Fraction | Exponent => provide(Token.NumberLiteral(BigDecimal(buffer.toString), Location(source, offset, line)))
                case _ => errorAt(offset, line, SyntaxErrorCode.UnsupportedNumberSyntax, "unsupported number syntax: %s", buffer)
            }
            
            def add(ch: Int) {
                buffer += ch.asInstanceOf[Char]
            }
            
            @tailrec def loop(md: Int, ch: Int): Boolean = ch match {
                case '+' | '-' => md match {
                    case BeforeSign => { if (ch != '+') add(ch); read; loop(AfterSign, peek) }
                    case AfterExpMarker => { if (ch != '+') add(ch); read; loop(AfterSign, peek) }
                    case _ => done(md)
                }
                case 'e' | 'E' => md match {
                    case Integer => { buffer ++= ".0e"; read; loop(AfterExpMarker, peek) }
                    case Fraction => { add('e'); read; loop(AfterExpMarker, peek) }
                    case _ => done(md)
                }
                case '.' => md match {
                    case Integer => { add('.'); read; loop(AfterDot, peek) }
                    case _ => done(md)
                }
                case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => md match {
                    case BeforeSign | AfterSign | Integer => { add(read); loop(Integer, peek) }
                    case AfterDot | Fraction => { add(read); loop(Fraction, peek) }
                    case AfterExpMarker | AfterExpSign | Exponent => { add(read); loop(Exponent, peek) }
                    case _ => done(md)
                }
                case _ => done(md)
            }
            
            loop(BeforeSign, peek)
        }
        
        @tailrec def expect(str: String, p: Int, ifMatch: =>Token): Boolean = {
            if (p == str.length()) provide(ifMatch)
            else {
                val offs = _offset
                val line = _line
                val ch = read
                if (ch == str.charAt(p)) expect(str, 1 + p, ifMatch)
                else badCharAt(offs, line)
            }
        }
        
    	@tailrec def mainloop(ch: Int, offset: Int, line: Int): Boolean = {
    	    ch match {
    	        case ' ' | '\n' | '\r' | '\t' => { read; mainloop(peek, _offset, _line) }
    	        case '{' => { read; provide(Token.LeftCurly(Location(source, offset, line))) }
    	        case '}' => { read; provide(Token.RightCurly(Location(source, offset, line))) }
    	        case '[' => { read; provide(Token.LeftBracket(Location(source, offset, line))) }
    	        case ']' => { read; provide(Token.RightBracket(Location(source, offset, line))) }
    	        case ',' => { read; provide(Token.Comma(Location(source, offset, line))) }
    	        case ':' => { read; provide(Token.Colon(Location(source, offset, line))) }
    	        case 'n' => expect("null", 0, Token.NullLiteral(Location(source, offset, line)))
    	        case 't' => expect("true", 0, Token.BooleanLiteral(true, Location(source, offset, line)))
    	        case 'f' => expect("false", 0, Token.BooleanLiteral(false, Location(source, offset, line)))
    	        case '+' | '-' | '0' | '1' | '2' | '3' => readNumber
    	        case '4' | '5' | '6' | '7' | '8' | '9' => readNumber
    	        case '"' => { read; readString }
    	        case k if k < 0 => eofAt(offset, line)
    	        case _ => badCharAt(offset, line)
    	    }
    	}
    	mainloop(peek, _offset, _line)
    }
    
    def toStream: Stream[Token] = {
        def advance: Stream[Token] = {
            if (next) Stream.cons(_token, advance) 
            else _token match {
                case _: Token.EndOfInput => Stream.empty
                case e: Token.SyntaxError => Stream.cons(e, Stream.empty)
                case _ => throw new AssertionError
            }
        }
        _token match {
            case InitialToken => advance
            case t => Stream.cons(t, advance)
        }
    }
}

class ReaderTokenizer (private val reader: Reader, val source: String) 
extends Tokenizer {
    
	protected def nextChar: Int = reader.read    
}

class StringTokenizer (private val data: String, start: Int, private val end: Int) 
extends Tokenizer {
    
    def this(data: String, start: Int) = this(data, start, data.length)
    def this(data: String) = this(data, 0, data.length)
    
    private var position: Int = start
    
    protected def source: String = "<string>"
    
    protected def nextChar: Int = 
        if (position >= end) -1 else {
            val ch = data.charAt(position)
            position += 1
            ch
        } 
}

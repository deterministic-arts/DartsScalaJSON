package darts.lib.json.read

object SyntaxErrorCode 
extends Enumeration {
    
    val IllegalLexerState = Value(0, "IllegalLexerState")
    val IllegalCharacter = Value(1, "IllegalCharacter")
    val UnterminatedStringLiteral = Value(2, "UnterminatedStringLiteral")
    val IllegalBackslashEscape = Value(3, "IllegalBackslashEscape")
    val UnsupportedNumberSyntax = Value(4, "UnsupportedNumberSyntax")
}


sealed trait Token {
	def location: Location
}

object Token {
    
    final case class SyntaxError (val code: SyntaxErrorCode.Value, val message: String, val location: Location) extends Token
    final case class EndOfInput (val location: Location) extends Token
    
    final case class LeftCurly (val location: Location) extends Token
    final case class RightCurly (val location: Location) extends Token
    final case class LeftBracket (val location: Location) extends Token
    final case class RightBracket (val location: Location) extends Token
    final case class Comma (val location: Location) extends Token
    final case class Colon (val location: Location) extends Token
    final case class StringLiteral (val value: String, val location: Location) extends Token
    final case class BooleanLiteral (val value: Boolean, val location: Location) extends Token
    final case class NumberLiteral (val value: BigDecimal, val location: Location) extends Token
    final case class NullLiteral (val location: Location) extends Token
}
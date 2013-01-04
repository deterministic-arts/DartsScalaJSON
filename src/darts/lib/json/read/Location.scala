package darts.lib.json.read

final case class Location (
	val source: String,
	val offset: Int,
	val line: Int
)

object Location {
    
    val Unknown = Location("<unknown>", 0, 1)
}
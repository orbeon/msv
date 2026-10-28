package java.text

class ParseException(message: String, private val errorOffset: Int) extends Exception(message) {
  def getErrorOffset: Int = errorOffset
}

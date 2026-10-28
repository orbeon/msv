package java.util

class SimpleTimeZone(rawOffset: Int, id: String) extends TimeZone {
  private var _id: String = id

  def getRawOffset: Int = rawOffset

  def getID: String = _id

  def setID(newId: String): Unit = {
    _id = newId
  }
}

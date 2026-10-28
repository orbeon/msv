package java.util

class SimpleTimeZone(rawOffset: Int, id: String) extends TimeZone {
  private var _id: String = id

  def getRawOffset: Int = rawOffset

  override def getID: String = _id

  override def setID(newId: String): Unit = {
    _id = newId
  }

  override def getDisplayName(daylight: Boolean, style: Int, locale: Locale): String = _id
  override def getDisplayName(daylight: Boolean, style: Int): String = _id
  override def getDisplayName(locale: Locale): String = _id
  override def getDisplayName: String = _id
}

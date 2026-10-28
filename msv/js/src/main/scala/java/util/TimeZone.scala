package java.util

abstract class TimeZone extends Serializable with Cloneable {
  private var _id: String = ""

  def getRawOffset: Int
  def getID: String = _id
  def setID(id: String): Unit = {
    _id = if (id != null) id else ""
  }
  def inDaylightTime(date: Date): Boolean = false
  def useDaylightTime(): Boolean = false
  def getDisplayName(daylight: Boolean, style: Int, locale: Locale): String = getID
  def getDisplayName(daylight: Boolean, style: Int): String = getID
  def getDisplayName(locale: Locale): String = getID
  def getDisplayName: String = getID
}

object TimeZone {
  def getTimeZone(id: String): TimeZone =
    new SimpleTimeZone(0, if (id != null) id else "")

  def getDefault: TimeZone =
    new SimpleTimeZone(0, "UTC")
}

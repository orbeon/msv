package java.util

abstract class TimeZone extends Serializable with Cloneable {
  def getRawOffset: Int
  def getID: String
  def setID(id: String): Unit
  def inDaylightTime(date: Date): Boolean = false
  def useDaylightTime(): Boolean = false
}

object TimeZone {
  def getTimeZone(id: String): TimeZone =
    new SimpleTimeZone(0, if (id != null) id else "")

  def getDefault: TimeZone =
    new SimpleTimeZone(0, "UTC")
}

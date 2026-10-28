package java.util

abstract class Calendar extends Serializable with Cloneable {
  protected val fields: Array[Int] = new Array[Int](Calendar.FIELD_COUNT)
  protected val isSet_ : Array[Boolean] = new Array[Boolean](Calendar.FIELD_COUNT)
  protected var zone: TimeZone = TimeZone.getDefault
  protected var isLenient_ : Boolean = true

  def clear(): Unit = {
    var i = 0
    while (i < Calendar.FIELD_COUNT) {
      fields(i) = 0
      isSet_(i) = false
      i += 1
    }
  }

  def clear(field: Int): Unit = {
    fields(field) = 0
    isSet_(field) = false
  }

  def isSet(field: Int): Boolean = isSet_(field)

  def set(field: Int, value: Int): Unit = {
    fields(field) = value
    isSet_(field) = true
  }

  def get(field: Int): Int = fields(field)

  def setTimeZone(value: TimeZone): Unit = {
    zone = value
  }

  def getTimeZone: TimeZone = zone

  def setLenient(lenient: Boolean): Unit = {
    isLenient_ = lenient
  }

  def isLenient: Boolean = isLenient_

  def getTime: Date = new Date(getTimeInMillis)

  def getTimeInMillis: Long = 0L
}

object Calendar {
  val ERA: Int = 0
  val YEAR: Int = 1
  val MONTH: Int = 2
  val WEEK_OF_YEAR: Int = 3
  val WEEK_OF_MONTH: Int = 4
  val DATE: Int = 5
  val DAY_OF_MONTH: Int = 5
  val DAY_OF_YEAR: Int = 6
  val DAY_OF_WEEK: Int = 7
  val DAY_OF_WEEK_IN_MONTH: Int = 8
  val AM_PM: Int = 9
  val HOUR: Int = 10
  val HOUR_OF_DAY: Int = 11
  val MINUTE: Int = 12
  val SECOND: Int = 13
  val MILLISECOND: Int = 14
  val ZONE_OFFSET: Int = 15
  val DST_OFFSET: Int = 16
  val FIELD_COUNT: Int = 17

  def getInstance(): Calendar = new GregorianCalendar()

  def getInstance(zone: TimeZone): Calendar = {
    val c = new GregorianCalendar()
    c.setTimeZone(zone)
    c
  }
}

class GregorianCalendar(year: Int, month: Int, dayOfMonth: Int) extends Calendar {
  def this() = this(0, 0, 0)
}

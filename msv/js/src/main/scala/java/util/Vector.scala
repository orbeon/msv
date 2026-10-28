package java.util

class Vector[E] extends ArrayList[E] {
  def this(initialCapacity: Int) = this()
  def this(initialCapacity: Int, capacityIncrement: Int) = this()
  def this(c: Collection[? <: E]) = {
    this()
    addAll(c)
  }

  def elementAt(index: Int): E = get(index)
  def addElement(obj: E): Unit = { add(obj); () }
  def removeElement(obj: Any): Boolean = remove(obj)
  def removeElementAt(index: Int): Unit = { remove(index); () }
  def removeAllElements(): Unit = clear()
  def elements(): Enumeration[E] = Collections.enumeration(this)
  def copyInto(anArray: Array[AnyRef]): Unit = {
    var i = 0
    while (i < size()) {
      anArray(i) = get(i).asInstanceOf[AnyRef]
      i += 1
    }
  }
}

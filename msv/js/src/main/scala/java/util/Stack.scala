package java.util

class Stack[E] extends Vector[E] {
  def push(item: E): E = {
    add(item)
    item
  }

  def pop(): E = {
    if (isEmpty) throw new EmptyStackException()
    remove(size() - 1)
  }

  def peek(): E = {
    if (isEmpty) throw new EmptyStackException()
    get(size() - 1)
  }

  def empty(): Boolean = isEmpty

  def search(o: Any): Int = {
    val i = lastIndexOf(o)
    if (i >= 0) size() - i else -1
  }
}

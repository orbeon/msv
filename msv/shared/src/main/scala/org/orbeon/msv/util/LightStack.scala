package org.orbeon.msv.util

/**
 * light-weight stack implementation.
 *
 * This one is unsynchronized, and never shrink its memory footprint, but fast.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
final class LightStack {
  private var buf: Array[AnyRef] = new Array[AnyRef](8)
  private var len: Int = 0

  def push(o: AnyRef): Unit = {
    if (len >= buf.length) {
      val nbuf = new Array[AnyRef](buf.length * 2)
      System.arraycopy(buf, 0, nbuf, 0, buf.length)
      buf = nbuf
    }
    buf(len) = o
    len += 1
  }

  def pop(): AnyRef = {
    len -= 1
    buf(len)
  }

  def top(): AnyRef = buf(len - 1)

  def size(): Int = len

  def contains(o: AnyRef): Boolean = {
    var i = 0
    while (i < len) {
      if (buf(i) == o) return true
      i += 1
    }
    false
  }
}

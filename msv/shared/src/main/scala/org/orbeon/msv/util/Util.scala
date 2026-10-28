package org.orbeon.msv.util

import java.io.File
import java.net.{URI, URL}
import org.xml.sax.InputSource

/**
 * Collection of utility methods.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
object Util {
  /**
   * Gets an InputSource from a string, which contains either
   * a file name or an URL.
   */
  def getInputSource(fileOrURL: String): InputSource = {
    try {
      new URI(fileOrURL)
      if (fileOrURL.contains(":"))
        new InputSource(fileOrURL)
      else
        throw new IllegalArgumentException()
    } catch {
      case _: Exception =>
        var path = new File(fileOrURL).getAbsolutePath
        if (File.separatorChar != '/')
          path = path.replace(File.separatorChar, '/')
        if (!path.startsWith("/"))
          path = "/" + path
        new InputSource("file://" + path)
    }
  }

  /**
   * Checks if a given string is an absolute URI if it is an URI.
   *
   * <p>
   * This method does not check whether it is an URI.
   *
   * <p>
   * This implementation is based on
   * <a href="http://lists.oasis-open.org/archives/relax-ng/200107/msg00211.html">
   * this post.</a>
   */
  def isAbsoluteURI(uri: String): Boolean = {
    val len = uri.length
    if (len == 0) return true
    if (len < 2) return false

    var ch = uri.charAt(0)
    if (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')) {
      var i = 1
      while (i < len) {
        ch = uri.charAt(i)
        if (ch == ':') return true
        if (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')) {
          i += 1
        } else if (ch == '-' || ch == '+' || ch == '.') {
          i += 1
        } else {
          return false
        }
      }
    }
    false
  }

  def which(clazz: Class[?]): String =
    which(clazz.getName, clazz.getClassLoader)

  /**
   * Search the specified classloader for the given classname.
   *
   * @param classname the fully qualified name of the class to search for
   * @param loader the classloader to search
   * @return the source location of the resource, or null if it wasn't found
   */
  def which(classname: String, loader: ClassLoader): String = {
    val classnameAsResource = classname.replace('.', '/') + ".class"
    val actualLoader = if (loader == null) ClassLoader.getSystemClassLoader else loader
    val it: URL = actualLoader.getResource(classnameAsResource)
    if (it != null) it.toString else null
  }
}

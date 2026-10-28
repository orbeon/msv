package org.orbeon.msv.relaxng.datatype.helpers

import org.orbeon.msv.relaxng.datatype.{DatatypeLibrary, DatatypeLibraryFactory}
import java.io.{BufferedReader, IOException, InputStream, InputStreamReader, Reader}
import java.net.URL
import java.util.{ArrayList as JArrayList, Collections, Enumeration, List as JList, NoSuchElementException}
import scala.compiletime.uninitialized

private[helpers] object DatatypeLibraryLoaderPlatform {

  private lazy val service1 = new Service(classOf[DatatypeLibraryFactory])
  private lazy val service2 = new Service("org.relaxng.datatype.DatatypeLibraryFactory")

  def createDatatypeLibrary(uri: String): DatatypeLibrary = {
    def findIn(service: Service): DatatypeLibrary = {
      val e = service.getProviders()
      while (e.hasMoreElements()) {
        val factory = e.nextElement().asInstanceOf[DatatypeLibraryFactory]
        val library = factory.createDatatypeLibrary(uri)
        if (library != null)
          return library
      }
      null
    }

    val lib1 = findIn(service1)
    if (lib1 != null) lib1
    else findIn(service2)
  }

  private class Service(serviceClassName: String, serviceClassOpt: Option[Class[?]]) {
    def this(cls: Class[?]) = this(cls.getName, Some(cls))
    def this(name: String)  = this(name, None)

    private val providers = new JArrayList[AnyRef]()
    private var loader: Loader = uninitialized
    try {
      loader = new Loader2()
    } catch {
      case _: NoSuchMethodError =>
        loader = new Loader()
    }

    private val resName = "META-INF/services/" + serviceClassName
    private val configFiles: Enumeration[URL] = loader.getResources(resName)
    private var classNames: Enumeration[String] = uninitialized

    private class ProviderEnumeration extends Enumeration[AnyRef] {
      private var nextIndex = 0

      def hasMoreElements(): Boolean =
        nextIndex < providers.size() || moreProviders()

      def nextElement(): AnyRef = {
        try {
          val res = providers.get(nextIndex)
          nextIndex += 1
          res
        } catch {
          case _: ArrayIndexOutOfBoundsException =>
            throw new NoSuchElementException()
        }
      }
    }

    private class Singleton[T](private var obj: T) extends Enumeration[T] {
      def hasMoreElements(): Boolean = obj != null

      def nextElement(): T = {
        if (obj == null)
          throw new NoSuchElementException()
        val tem = obj
        obj = null.asInstanceOf[T]
        tem
      }
    }

    private class Loader {
      def getResources(resName: String): Enumeration[URL] = {
        val cl = classOf[Loader].getClassLoader
        val url = if (cl == null) ClassLoader.getSystemResource(resName) else cl.getResource(resName)
        new Singleton[URL](url)
      }

      @throws(classOf[ClassNotFoundException])
      def loadClass(name: String): Class[?] =
        Class.forName(name)
    }

    private class Loader2 extends Loader {
      private var cl: ClassLoader = classOf[Loader2].getClassLoader
      val clt = Thread.currentThread().getContextClassLoader
      var tem = clt
      var found = false
      while (tem != null && !found) {
        if (tem == cl) {
          cl = clt
          found = true
        } else {
          tem = tem.getParent
        }
      }

      override def getResources(resName: String): Enumeration[URL] = {
        if (cl == null)
          return super.getResources(resName)
        try {
          cl.getResources(resName)
        } catch {
          case _: IOException =>
            new Singleton[URL](null)
        }
      }

      @throws(classOf[ClassNotFoundException])
      override def loadClass(name: String): Class[?] = {
        if (cl == null)
          return super.loadClass(name)
        Class.forName(name, true, cl)
      }
    }

    def getProviders(): Enumeration[AnyRef] =
      new ProviderEnumeration()

    private def moreProviders(): Boolean = synchronized {
      while (true) {
        while (classNames == null) {
          if (!configFiles.hasMoreElements())
            return false
          classNames = parseConfigFile(configFiles.nextElement())
        }
        while (classNames.hasMoreElements()) {
          val className = classNames.nextElement()
          try {
            val cls = loader.loadClass(className)
            val obj = cls.getDeclaredConstructor().newInstance().asInstanceOf[AnyRef]
            val isInst = serviceClassOpt match {
              case Some(sc) => sc.isInstance(obj)
              case None     => obj.isInstanceOf[DatatypeLibraryFactory]
            }
            if (isInst) {
              providers.add(obj)
              return true
            }
          } catch {
            case _: ClassNotFoundException =>
            case _: InstantiationException =>
            case _: IllegalAccessException =>
            case _: LinkageError           =>
            case _: NoSuchMethodException  =>
          }
        }
        classNames = null
      }
      false
    }

    private val START = 0
    private val IN_NAME = 1
    private val IN_COMMENT = 2

    private def parseConfigFile(url: URL): Enumeration[String] = {
      if (url == null)
        return new Singleton[String](null)
      try {
        val in = url.openStream()
        val r: Reader = new BufferedReader(new InputStreamReader(in, "UTF-8"))
        val tokens = new JArrayList[String]()
        val tokenBuf = new java.lang.StringBuilder()
        var state = START
        var done = false
        while (!done) {
          val n = r.read()
          if (n < 0) {
            done = true
          } else {
            val c = n.toChar
            c match {
              case '\r' | '\n' =>
                state = START
              case ' ' | '\t' =>
              case '#' =>
                state = IN_COMMENT
              case _ =>
                if (state != IN_COMMENT) {
                  state = IN_NAME
                  tokenBuf.append(c)
                }
            }
            if (tokenBuf.length() != 0 && state != IN_NAME) {
              tokens.add(tokenBuf.toString())
              tokenBuf.setLength(0)
            }
          }
        }
        if (tokenBuf.length() != 0)
          tokens.add(tokenBuf.toString())
        Collections.enumeration(tokens)
      } catch {
        case _: IOException =>
          new Singleton[String](null)
      }
    }
  }
}

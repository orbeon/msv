package org.orbeon.msv.iso_relax.verifier

private[verifier] object VerifierFactoryPlatform:

  def findFactory(language: String): Option[VerifierFactory] =
    try
      val cl = getClass.getClassLoader
      val sl = java.util.ServiceLoader.load(classOf[VerifierFactoryLoader], cl)
      val it = sl.iterator()
      while it.hasNext do
        try
          val factory = it.next().createFactory(language)
          if factory != null then return Some(factory)
        catch
          case _: Throwable =>
      None
    catch
      case _: Throwable =>
        None

  def findFactory(language: String, classLoader: ClassLoader): Option[VerifierFactory] =
    try
      val sl = java.util.ServiceLoader.load(classOf[VerifierFactoryLoader], classLoader)
      val it = sl.iterator()
      while it.hasNext do
        try
          val factory = it.next().createFactory(language)
          if factory != null then return Some(factory)
        catch
          case _: Throwable =>
      None
    catch
      case _: Throwable =>
        None

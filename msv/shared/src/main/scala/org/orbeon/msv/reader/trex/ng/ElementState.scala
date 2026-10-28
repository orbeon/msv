package org.orbeon.msv.reader.trex.ng

/**
 * parses &lt;element&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementState extends org.orbeon.msv.reader.trex.ElementState:
  private var previousDirectReference: Boolean = false

  protected override def startSelf(): Unit =
    super.startSelf()
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    previousDirectReference = rngReader.directReference
    rngReader.directReference = false

  protected override def endSelf(): Unit =
    val rngReader = reader.asInstanceOf[RELAXNGReader]
    rngReader.directReference = previousDirectReference
    super.endSelf()
    rngReader.restrictionChecker.checkNameClass(nameClass)

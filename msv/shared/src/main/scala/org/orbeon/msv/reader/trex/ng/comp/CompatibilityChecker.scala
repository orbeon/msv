package org.orbeon.msv.reader.trex.ng.comp

import org.orbeon.msv.grammar.relaxng.RELAXNGGrammar
import org.xml.sax.Locator

abstract class CompatibilityChecker(protected val reader: RELAXNGCompReader):
  protected val grammar: RELAXNGGrammar = reader.getGrammar.asInstanceOf[RELAXNGGrammar]

  protected def setCompatibility(val_ : Boolean): Unit

  protected def reportCompError(locs: Array[Locator], propertyName: String): Unit =
    reportCompError(locs, propertyName, null)

  protected def reportCompError(locs: Array[Locator], propertyName: String, args: Array[AnyRef]): Unit =
    setCompatibility(false)
    reader.reportWarning(propertyName, args, locs)

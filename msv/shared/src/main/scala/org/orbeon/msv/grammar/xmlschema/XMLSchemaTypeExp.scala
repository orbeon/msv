package org.orbeon.msv.grammar.xmlschema

/**
 * Base class of [[ComplexTypeExp]] and [[SimpleTypeExp]].
 * 
 * This class represents "type" of W3C XML Schema.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class XMLSchemaTypeExp(typeLocalName: String) extends RedefinableExp(typeLocalName):

  /**
   * gets the value of the block constraint.
   */
  def getBlock: Int

object XMLSchemaTypeExp:
  val RESTRICTION: Int = 0x1
  val EXTENSION: Int = 0x2

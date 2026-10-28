package org.orbeon.msv.grammar.xmlschema

import java.util.{Iterator as JIterator, Map as JMap}
import org.orbeon.msv.grammar.{Expression, ExpressionPool, Grammar}

/**
 * set of XML Schema. This set can be used to validate a document.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class XMLSchemaGrammar(val pool: ExpressionPool) extends Grammar:

  def this() = this(new ExpressionPool())

  def getPool: ExpressionPool = pool

  var topLevel: Expression = scala.compiletime.uninitialized
  final def getTopLevel: Expression = topLevel

  protected[xmlschema] val schemata: JMap[String, XMLSchemaSchema] = new java.util.HashMap[String, XMLSchemaSchema]()

  def getByNamespace(targetNamespace: String): XMLSchemaSchema =
    schemata.get(targetNamespace)

  def iterateSchemas(): JIterator[XMLSchemaSchema] =
    schemata.values().iterator()

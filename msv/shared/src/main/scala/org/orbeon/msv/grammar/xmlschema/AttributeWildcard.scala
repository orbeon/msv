package org.orbeon.msv.grammar.xmlschema

import java.util.Iterator as JIterator
import org.orbeon.msv.grammar.{Expression, NameClass, ReferenceExp}

/**
 * Attribute wildcard property of the schema component.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeWildcard(private val name: NameClass, private val processMode: Int) extends Serializable:

  def getName: NameClass = name

  def getProcessMode: Int = processMode

  def copy(): AttributeWildcard =
    new AttributeWildcard(name, processMode)

  def createExpression(grammar: XMLSchemaGrammar): Expression =
    val pool = grammar.pool

    processMode match
      case AttributeWildcard.SKIP =>
        pool.createZeroOrMore(pool.createAttribute(name))

      case AttributeWildcard.STRICT | AttributeWildcard.LAX =>
        var exp: Expression = Expression.epsilon
        val laxNc = new LaxDefaultNameClass(name)

        val itr: JIterator[?] = grammar.iterateSchemas()
        while itr.hasNext do
          val schema = itr.next().asInstanceOf[XMLSchemaSchema]
          if name.accepts(schema.targetNamespace, NameClass.LOCALNAME_WILDCARD) then
            val atts = schema.attributeDecls.getAll
            var i = 0
            while i < atts.length do
              exp = pool.createSequence(pool.createOptional(atts(i)), exp)
              laxNc.addName(schema.targetNamespace, atts(i).name)
              i += 1

        if processMode == AttributeWildcard.STRICT then
          exp
        else
          pool.createSequence(pool.createZeroOrMore(pool.createAttribute(laxNc)), exp)

      case _ =>
        throw new Error("undefined process mode: " + processMode)

object AttributeWildcard:
  val SKIP: Int = 0
  val LAX: Int = 1
  val STRICT: Int = 2

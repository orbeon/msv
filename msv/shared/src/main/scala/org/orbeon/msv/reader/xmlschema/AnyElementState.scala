package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.trex.ElementPattern
import org.orbeon.msv.grammar.xmlschema.ElementDeclExp
import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema
import org.orbeon.msv.reader.GrammarReader
import org.orbeon.msv.reader.State

/**
 * Used to parse &lt;any &gt; element.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AnyElementState extends AnyState:

  override protected def createExpression(namespace: String, process: String): Expression =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val currentSchema = reader.currentSchema

    if process == "skip" then
      val nc = getNameClass(namespace, currentSchema)
      val ep = new ElementPattern(nc, Expression.nullSet)

      ep.contentModel =
        reader.pool.createMixed(
          reader.pool.createZeroOrMore(
            reader.pool.createChoice(
              ep,
              reader.pool.createAttribute(nc)
            )
          )
        )

      ep
    else
      val exp = new ReferenceExp(s"any($process:$namespace)")
      reader.addBackPatchJob(new GrammarReader.BackPatch:
        def getOwnerState: State = AnyElementState.this
        def patch(): Unit =
          if process != "lax" && process != "strict" then
            reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "processContents", process)
            exp.exp = Expression.nullSet
            return

          exp.exp = Expression.nullSet
          val nc = getNameClass(namespace, currentSchema)
          val itr = reader.grammar.iterateSchemas()
          while itr.hasNext do
            val schema = itr.next().asInstanceOf[XMLSchemaSchema]
            if nc.accepts(schema.targetNamespace, NameClass.LOCALNAME_WILDCARD) then
              exp.exp = reader.pool.createChoice(exp.exp, schema.topLevel)

          if process != "lax" then
            return

          val laxNc = createLaxNameClass(
            nc,
            new XMLSchemaReader.RefResolver:
              def get(schema: XMLSchemaSchema): ReferenceContainer = schema.elementDecls
          )

          exp.exp = reader.pool.createChoice(
            new ElementPattern(
              laxNc,
              reader.pool.createMixed(
                reader.pool.createZeroOrMore(
                  reader.pool.createChoice(
                    reader.pool.createAttribute(NameClass.ALL),
                    exp
                  )
                )
              )
            ),
            exp.exp
          )
      )

      exp.exp = Expression.nullSet
      exp

  override protected def getNameClassFrom(exp: ReferenceExp): NameClass =
    exp.asInstanceOf[ElementDeclExp].getElementExp.getNameClass

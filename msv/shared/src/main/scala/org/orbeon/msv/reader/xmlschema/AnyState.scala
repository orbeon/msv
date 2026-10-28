package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.xmlschema.LaxDefaultNameClass
import org.orbeon.msv.grammar.xmlschema.XMLSchemaSchema
import org.orbeon.msv.reader.ExpressionWithoutChildState

/**
 * Base implementation of AnyAttributeState and AnyElementState.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class AnyState extends ExpressionWithoutChildState:

  protected final def makeExpression(): Expression =
    createExpression(
      startTag.getDefaultedAttribute("namespace", "##any"),
      startTag.getDefaultedAttribute("processContents", "strict")
    )

  /**
   * Creates AGM that corresponds to the specified parameters.
   */
  protected def createExpression(namespace: String, process: String): Expression

  /**
   * Processes 'namespace' attribute and gets corresponding NameClass object.
   */
  protected def getNameClass(namespaceParam: String, currentSchema: XMLSchemaSchema): NameClass =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val trimmed = namespaceParam.trim

    if trimmed == "##any" then
      NameClass.ALL
    else if trimmed == "##other" then
      // ##other means anything other than the target namespace and local.
      new NotNameClass(
        new ChoiceNameClass(
          new NamespaceNameClass(currentSchema.targetNamespace),
          new NamespaceNameClass("")
        )
      )
    else
      var choices: NameClass = null
      val tokens = trimmed.split("\\s+").filter(_.nonEmpty)
      for token <- tokens do
        val nc: NameClass =
          if token == "##targetNamespace" then
            new NamespaceNameClass(currentSchema.targetNamespace)
          else if token == "##local" then
            new NamespaceNameClass("")
          else
            new NamespaceNameClass(token)

        if choices == null then
          choices = nc
        else
          choices = new ChoiceNameClass(choices, nc)

      if choices == null then
        reader.reportError(XMLSchemaReader.ERR_BAD_ATTRIBUTE_VALUE, "namespace", trimmed)
        NameClass.ALL
      else
        choices

  protected def getNameClassFrom(exp: ReferenceExp): NameClass

  protected def createLaxNameClass(allowedNc: NameClass, res: XMLSchemaReader.RefResolver): NameClass =
    val reader = this.reader.asInstanceOf[XMLSchemaReader]
    val laxNc = new LaxDefaultNameClass(allowedNc)

    val itr = reader.grammar.iterateSchemas()
    while itr.hasNext do
      val schema = itr.next().asInstanceOf[XMLSchemaSchema]
      if allowedNc.accepts(schema.targetNamespace, NameClass.LOCALNAME_WILDCARD) then
        val refs = res.get(schema).getAll
        for i <- refs.indices do
          val name = getNameClassFrom(refs(i))
          if !name.isInstanceOf[SimpleNameClass] then
            throw new Error()
          val snc = name.asInstanceOf[SimpleNameClass]
          laxNc.addName(snc.namespaceURI, snc.localName)

    new DifferenceNameClass(laxNc, new NotNameClass(allowedNc))

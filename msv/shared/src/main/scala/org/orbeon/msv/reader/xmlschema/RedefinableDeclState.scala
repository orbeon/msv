package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.ReferenceContainer
import org.orbeon.msv.grammar.xmlschema.RedefinableExp
import org.orbeon.msv.reader.ExpressionWithChildState

/**
 * State that parses redefinable declaration.
 *
 * "Declarations" are attribute, element, complexType, group, and attributeGroup.
 * simpleType is treated differently.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
abstract class RedefinableDeclState extends ExpressionWithChildState:

  protected def isGlobal(): Boolean =
    parentState.isInstanceOf[GlobalDeclState]

  /**
   * Returns true if this declaration is a redefinition of an
   * existing declaration.
   */
  protected def isRedefine(): Boolean =
    parentState.isInstanceOf[RedefineState]

  /**
   * Keeps a reference to previous declaration.
   *
   * This field is used only when in redefine mode. Derived class should use
   * this declaration instead of getting one from ReferenceContainer through
   * XMLSchemaSchema.
   */
  protected var oldDecl: RedefinableExp = null

  /** Gets appropriate ReferenceContainer to store this declaration. */
  protected def getContainer(): ReferenceContainer

  override protected def startSelf(): Unit =
    super.startSelf()

    if isRedefine() then
      val reader = this.reader.asInstanceOf[XMLSchemaReader]

      val name = startTag.getAttribute("name")
      if name == null then
        // ignore this error just for now.
        // this error will be reported in annealExpression method.
        return

      oldDecl = getContainer()._get(name).asInstanceOf[RedefinableExp]
      if oldDecl == null then
        reader.reportError(XMLSchemaReader.ERR_REDEFINE_UNDEFINED, name)
        // recover by creating a dummy object.
        oldDecl = getContainer()._getOrCreate(name).asInstanceOf[RedefinableExp]
        return

      getContainer().redefine(name, oldDecl.getClone)

  override protected def endSelf(): Unit =
    if oldDecl != null then
      getContainer().redefine(oldDecl.name, oldDecl)

    super.endSelf()

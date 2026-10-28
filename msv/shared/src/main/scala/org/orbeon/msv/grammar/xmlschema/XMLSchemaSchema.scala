package org.orbeon.msv.grammar.xmlschema

import java.util.Map as JMap
import org.orbeon.msv.grammar.{Expression, ExpressionPool, ReferenceContainer, ReferenceExp}

/**
 * XML Schema object.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class XMLSchemaSchema(val targetNamespace: String, parent: XMLSchemaGrammar) extends Serializable:

  val pool: ExpressionPool = parent.pool
  parent.schemata.put(targetNamespace, this)

  var topLevel: Expression = scala.compiletime.uninitialized

  final class SimpleTypeContainer extends ReferenceContainer:
    def getOrCreate(name: String): SimpleTypeExp =
      super._getOrCreate(name).asInstanceOf[SimpleTypeExp]

    def get(name: String): SimpleTypeExp =
      super._get(name).asInstanceOf[SimpleTypeExp]

    protected def createReference(name: String): ReferenceExp =
      new SimpleTypeExp(name)

  val simpleTypes: SimpleTypeContainer = new SimpleTypeContainer()

  final class ComplexTypeContainer extends ReferenceContainer:
    def getOrCreate(name: String): ComplexTypeExp =
      super._getOrCreate(name).asInstanceOf[ComplexTypeExp]

    def get(name: String): ComplexTypeExp =
      super._get(name).asInstanceOf[ComplexTypeExp]

    protected def createReference(name: String): ReferenceExp =
      new ComplexTypeExp(XMLSchemaSchema.this, name)

  val complexTypes: ComplexTypeContainer = new ComplexTypeContainer()

  final class AttributeGroupContainer extends ReferenceContainer:
    def getOrCreate(name: String): AttributeGroupExp =
      super._getOrCreate(name).asInstanceOf[AttributeGroupExp]

    def get(name: String): AttributeGroupExp =
      super._get(name).asInstanceOf[AttributeGroupExp]

    protected def createReference(name: String): ReferenceExp =
      new AttributeGroupExp(name)

  val attributeGroups: AttributeGroupContainer = new AttributeGroupContainer()

  final class AttributeDeclContainer extends ReferenceContainer:
    def getOrCreate(name: String): AttributeDeclExp =
      super._getOrCreate(name).asInstanceOf[AttributeDeclExp]

    def get(name: String): AttributeDeclExp =
      super._get(name).asInstanceOf[AttributeDeclExp]

    protected def createReference(name: String): ReferenceExp =
      new AttributeDeclExp(name)

  val attributeDecls: AttributeDeclContainer = new AttributeDeclContainer()

  final class ElementDeclContainer extends ReferenceContainer:
    def getOrCreate(name: String): ElementDeclExp =
      super._getOrCreate(name).asInstanceOf[ElementDeclExp]

    def get(name: String): ElementDeclExp =
      super._get(name).asInstanceOf[ElementDeclExp]

    protected def createReference(name: String): ReferenceExp =
      new ElementDeclExp(XMLSchemaSchema.this, name)

  val elementDecls: ElementDeclContainer = new ElementDeclContainer()

  final class GroupDeclContainer extends ReferenceContainer:
    def getOrCreate(name: String): GroupDeclExp =
      super._getOrCreate(name).asInstanceOf[GroupDeclExp]

    def get(name: String): GroupDeclExp =
      super._get(name).asInstanceOf[GroupDeclExp]

    protected def createReference(name: String): ReferenceExp =
      new GroupDeclExp(name)

  val groupDecls: GroupDeclContainer = new GroupDeclContainer()

  final class IdentityConstraintContainer extends Serializable:
    private val storage: JMap[String, IdentityConstraint] = new java.util.HashMap[String, IdentityConstraint]()

    def get(name: String): IdentityConstraint = storage.get(name)

    def add(name: String, idc: IdentityConstraint): Unit =
      storage.put(name, idc)

  val identityConstraints: IdentityConstraintContainer = new IdentityConstraintContainer()

object XMLSchemaSchema:
  val XMLSchemaInstanceNamespace: String = "http://www.w3.org/2001/XMLSchema-instance"

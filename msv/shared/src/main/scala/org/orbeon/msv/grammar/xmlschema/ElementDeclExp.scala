package org.orbeon.msv.grammar.xmlschema

import java.util.Vector as JVector
import org.orbeon.msv.grammar.*
import org.orbeon.msv.grammar.util.ExpressionWalker

/**
 * Element declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class ElementDeclExp(val parent: XMLSchemaSchema, typeLocalName: String) extends ReferenceExp(typeLocalName):

  val substitutions: ReferenceExp = new ReferenceExp(typeLocalName + ":substitutions", Expression.nullSet)

  var substitutionAffiliation: ElementDeclExp = scala.compiletime.uninitialized

  val body: ReferenceExp = new ReferenceExp(null)

  private var element: ElementDeclExp#XSElementExp = scala.compiletime.uninitialized

  def setElementExp(exp: ElementDeclExp#XSElementExp): Unit =
    this.element = exp
    body.exp = exp

  def getElementExp: ElementDeclExp#XSElementExp = element

  def getContentModel: Expression = element.contentModel

  class XSElementExp(val elementName: SimpleNameClass, contentModel: Expression)
    extends ElementExp(contentModel, false):

    val parent: ElementDeclExp = ElementDeclExp.this

    override def getNameClass: NameClass = elementName

    val identityConstraints: JVector[IdentityConstraint] = new JVector[IdentityConstraint]()

  var isNillable: Boolean = false

  def isGlobal: Boolean = parent.elementDecls.get(name) eq this

  final def getTargetNamespace: String = parent.targetNamespace

  def isAbstract: Boolean =
    if exp.isInstanceOf[ChoiceExp] then
      val cexp = exp.asInstanceOf[ChoiceExp]
      if (cexp.exp1 ne body) && (cexp.exp2 ne body) then throw new Error()
      return true

    if exp ne substitutions then throw new Error()
    false

  def setAbstract(isAbstractVal: Boolean): Unit =
    if isAbstractVal then exp = substitutions
    else exp = parent.pool.createChoice(substitutions, body)

  var finalValue: Int = 0
  var block: Int = 0

  def isSubstitutionBlocked: Boolean = (block & ElementDeclExp.SUBSTITUTION) != 0
  def isRestrictionBlocked: Boolean = (block & ElementDeclExp.RESTRICTION) != 0

  def getTypeDefinition: XMLSchemaTypeExp =
    val eureka = new RuntimeException()
    val result = new Array[XMLSchemaTypeExp](1)
    try
      getContentModel.visit(new ExpressionWalker:
        override def onElement(exp: ElementExp): Unit = ()
        override def onRef(exp: ReferenceExp): Unit =
          if exp.isInstanceOf[XMLSchemaTypeExp] then
            result(0) = exp.asInstanceOf[XMLSchemaTypeExp]
            throw eureka
          super.onRef(exp)
      )
      throw new Error()
    catch
      case e: RuntimeException if e eq eureka => result(0)

  override def isDefined: Boolean = super.isDefined && element != null

object ElementDeclExp:
  type XSElementExp = ElementDeclExp#XSElementExp

  val RESTRICTION: Int = 0x1
  val EXTENSION: Int = 0x2
  val SUBSTITUTION: Int = 0x4
  val ALL: Int = 0x7

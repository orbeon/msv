package org.orbeon.msv.grammar.relax

import java.util.Map
import org.orbeon.msv.datatype.xsd.XSDatatype
import org.orbeon.msv.grammar.{Expression, ExpressionPool, Grammar, ReferenceContainer, ReferenceExp}

/**
 * "Module" of RELAX Core.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RELAXModule(val pool: ExpressionPool, val targetNamespace: String) extends Grammar:

  if targetNamespace == null then throw new NullPointerException()

  final class ElementRulesContainer extends ReferenceContainer:
    def getOrCreate(name: String): ElementRules =
      super._getOrCreate(name).asInstanceOf[ElementRules]
    def get(name: String): ElementRules =
      super._get(name).asInstanceOf[ElementRules]
    protected def createReference(name: String): ReferenceExp =
      new ElementRules(name, RELAXModule.this)

  /** map from label name to ElementRules object */
  val elementRules: ElementRulesContainer = new ElementRulesContainer()

  final class HedgeRulesContainer extends ReferenceContainer:
    def getOrCreate(name: String): HedgeRules =
      super._getOrCreate(name).asInstanceOf[HedgeRules]
    def get(name: String): HedgeRules =
      super._get(name).asInstanceOf[HedgeRules]
    protected def createReference(name: String): ReferenceExp =
      new HedgeRules(name, RELAXModule.this)

  /** map from label name to HedgeRules object */
  val hedgeRules: HedgeRulesContainer = new HedgeRulesContainer()

  final class TagContainer extends ReferenceContainer:
    def getOrCreate(name: String): TagClause =
      super._getOrCreate(name).asInstanceOf[TagClause]
    def get(name: String): TagClause =
      super._get(name).asInstanceOf[TagClause]
    protected def createReference(name: String): ReferenceExp =
      new TagClause(name)

  /** map from role name to TagClause object */
  val tags: TagContainer = new TagContainer()

  final class AttPoolContainer extends ReferenceContainer:
    def getOrCreate(name: String): AttPoolClause =
      super._getOrCreate(name).asInstanceOf[AttPoolClause]
    def get(name: String): AttPoolClause =
      super._get(name).asInstanceOf[AttPoolClause]
    protected def createReference(name: String): ReferenceExp =
      new AttPoolClause(name)

  /** map from role name to AttPoolClause object */
  val attPools: AttPoolContainer = new AttPoolContainer()

  /** Datatypes. */
  class DatatypeContainer:
    private val m: Map[String, XSDatatype] = new java.util.HashMap[String, XSDatatype]()

    def get(name: String): XSDatatype = m.get(name)

    def add(dt: XSDatatype): Unit =
      if dt.getName == null then throw new IllegalArgumentException()
      m.put(dt.getName, dt)

  val datatypes: DatatypeContainer = new DatatypeContainer()
  datatypes.add(EmptyStringType.theInstance)
  datatypes.add(NoneType.theInstance)

  /**
   * choice of all exported elementRules and hedgeRules.
   * 
   * This can be used as the top-level expression when a module is used
   * to validate documents by itself.
   */
  var topLevel: Expression = scala.compiletime.uninitialized
  def getTopLevel: Expression = topLevel

  def getPool: ExpressionPool = pool

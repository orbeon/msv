package org.orbeon.msv.grammar.trex

import org.orbeon.msv.grammar.{Expression, ExpressionPool, Grammar, ReferenceContainer, ReferenceExp}
import org.orbeon.msv.reader.datatype.DataTypeVocabularyMap

/**
 * TREX grammar, which is expressed as &lt;grammar&gt; element.
 * 
 * The `exp` field keeps the start pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TREXGrammar(val pool: ExpressionPool, protected val parentGrammar: TREXGrammar)
  extends ReferenceExp(null) with Grammar:

  def this(pool: ExpressionPool) = this(pool, null)
  def this() = this(new ExpressionPool(), null)

  val namedPatterns: TREXGrammar.RefContainer = new TREXGrammar.RefContainer()

  /** gets the start pattern. */
  def getTopLevel: Expression = this.exp

  def getPool: ExpressionPool = pool

  def getParentGrammar: TREXGrammar = parentGrammar

  /** data type vocabularies used and defined by this grammar. */
  val dataTypes: DataTypeVocabularyMap = new DataTypeVocabularyMap()

object TREXGrammar:
  final class RefContainer extends ReferenceContainer:
    def getOrCreate(name: String): ReferenceExp =
      super._getOrCreate(name)

    protected def createReference(name: String): ReferenceExp =
      new ReferenceExp(name)

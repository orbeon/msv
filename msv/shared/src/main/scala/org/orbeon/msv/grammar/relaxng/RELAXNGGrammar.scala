package org.orbeon.msv.grammar.relaxng

import org.orbeon.msv.grammar.ExpressionPool
import org.orbeon.msv.grammar.trex.TREXGrammar

/**
 * Grammar for RELAX NG (with DTD compatibility annotation).
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class RELAXNGGrammar(pool: ExpressionPool, parentGrammar: TREXGrammar)
  extends TREXGrammar(pool, parentGrammar):

  def this(pool: ExpressionPool) = this(pool, null)
  def this() = this(new ExpressionPool(), null)

  /**
   * the compatibility property of the ID/IDREF feature.
   */
  var isIDcompatible: Boolean = true

  /**
   * the compatibility property of the default attribute feature.
   */
  var isDefaultAttributeValueCompatible: Boolean = true

  /**
   * the compatibility property of the annotation feature.
   */
  var isAnnotationCompatible: Boolean = true

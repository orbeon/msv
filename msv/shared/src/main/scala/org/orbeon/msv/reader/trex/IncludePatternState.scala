package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.Expression
import org.orbeon.msv.reader.{AbortException, ExpressionOwner, ExpressionWithoutChildState}

/**
 * &lt;include&gt; element in the pattern. (&lt;externalRef&gt; of RELAX NG).
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class IncludePatternState extends ExpressionWithoutChildState with ExpressionOwner:
  protected var included: Expression = Expression.nullSet

  def onEndChild(included: Expression): Unit =
    this.included = included

  protected def makeExpression(): Expression =
    val href = startTag.getAttribute("href")
    if href == null then
      reader.reportError(org.orbeon.msv.reader.GrammarReader.ERR_MISSING_ATTRIBUTE, startTag.localName, "href")
      return Expression.nullSet

    try
      reader.switchSource(this, href, new RootIncludedPatternState(this))
    catch
      case _: AbortException =>

    included

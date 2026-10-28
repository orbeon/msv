package org.orbeon.msv.reader.trex

import org.orbeon.msv.grammar.{AttributeExp, Expression}

/**
 * parses &lt;attribute&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class AttributeState extends NameClassAndExpressionState:
  protected var firstChild: Boolean = true

  protected override def initialExpression(): Expression =
    // <attribute> defaults to <anyString />
    Expression.anyString

  protected override def getNamespace(): String =
    val ns = startTag.getAttribute("ns")
    val global = "true" == startTag.getAttribute("global")

    if ns != null then ns // "ns" attribute always has precedence.
    else if global then reader.asInstanceOf[TREXBaseReader].targetNamespace
    else ""

  protected override def castExpression(initialExpression: Expression, newChild: Expression): Expression =
    if !firstChild then
      reader.reportError(org.orbeon.msv.reader.GrammarReader.ERR_MORE_THAN_ONE_CHILD_EXPRESSION)
    firstChild = false
    newChild

  protected override def annealExpression(contentModel: Expression): Expression =
    val e = reader.pool.createAttribute(nameClass, contentModel)
    if e.isInstanceOf[AttributeExp] then
      reader.setDeclaredLocationOf(e)
    e

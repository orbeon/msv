package org.orbeon.msv.reader.trex.ng.comp

import org.orbeon.msv.grammar.{AttributeExp, Expression}
import org.orbeon.msv.reader.trex.ng.AttributeState

/**
 * parses &lt;attribute&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class CompAttributeState extends AttributeState:
  protected override def annealExpression(contentModel: Expression): Expression =
    val exp = super.annealExpression(contentModel)

    val defaultValue = startTag.getAttribute(
      RELAXNGCompReader.AnnotationNamespace, "defaultValue"
    )
    if defaultValue != null && exp.isInstanceOf[AttributeExp] then
      val compReader = reader.asInstanceOf[RELAXNGCompReader]
      compReader.addDefaultValue(exp.asInstanceOf[AttributeExp], defaultValue)

    exp

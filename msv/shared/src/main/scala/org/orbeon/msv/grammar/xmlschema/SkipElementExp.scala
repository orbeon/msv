package org.orbeon.msv.grammar.xmlschema

import org.orbeon.msv.grammar.{Expression, NameClass}
import org.orbeon.msv.grammar.trex.ElementPattern

/**
 * ElementExp that is used for &lt;any processContents="skip"/&gt;.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class SkipElementExp(nameClass: NameClass, contentModel: Expression)
  extends ElementPattern(nameClass, contentModel)

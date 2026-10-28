package org.orbeon.msv.reader.xmlschema

import org.orbeon.msv.grammar.xmlschema.AttributeWildcard

/**
 * A state that can have &lt;anyAttribute &gt; element must implement this interface.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait AnyAttributeOwner:
  /** Sets the attribute wildcard. */
  def setAttributeWildcard(local: AttributeWildcard): Unit

package org.orbeon.msv.grammar.trex.typed

import org.orbeon.msv.grammar.{Expression, NameClass}
import org.orbeon.msv.grammar.trex.ElementPattern

/**
 * ElementPattern with type.
 *
 * Proprietary extension by MSV to support type-assignment in TREX.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class TypedElementPattern(nameClass: NameClass, contentModel: Expression, val label: String)
  extends ElementPattern(nameClass, contentModel)

package org.orbeon.msv.grammar

import org.orbeon.msv.relaxng.datatype.{Datatype, ValidationContext}

/**
 * ValidationContextProvider that supports limited ID/IDREF implementation.
 *
 * @deprecated use [[IDContextProvider2]].
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
@deprecated("use IDContextProvider2", "")
trait IDContextProvider extends ValidationContext {
  /**
   * this method is called when a type with ID semantics is matched.
   *
   * It is the callee's responsibility that stores
   * ID and checks doubly defined ID, if it is necessary.
   */
  def onID(datatype: Datatype, literal: String): Unit
}

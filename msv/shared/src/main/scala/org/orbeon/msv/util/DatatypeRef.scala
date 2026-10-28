package org.orbeon.msv.util

import org.orbeon.msv.relaxng.datatype.Datatype

/**
 * reference to [[org.orbeon.msv.relaxng.datatype.Datatype]]
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DatatypeRef {
  /**
   * if the size of the array is zero, then that means this token is ignored.
   */
  var types: Array[Datatype] = null
}

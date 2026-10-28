package org.orbeon.msv.grammar

import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.util.StringPair

/**
 * The common interface for [[DataExp]] and [[ValueExp]].
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
trait DataOrValueExp {
  def getName: StringPair
  def getType: Datatype
}

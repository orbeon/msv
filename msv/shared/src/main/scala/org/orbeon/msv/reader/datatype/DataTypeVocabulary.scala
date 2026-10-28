package org.orbeon.msv.reader.datatype

import org.orbeon.msv.reader.State
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeException}
import org.orbeon.msv.util.StartTagInfo

/**
 * this class is used to parse foreign datatype vocabulary.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait DataTypeVocabulary:
  def createTopLevelReaderState(tag: StartTagInfo): State

  @throws(classOf[DatatypeException])
  def getType(localTypeName: String): Datatype

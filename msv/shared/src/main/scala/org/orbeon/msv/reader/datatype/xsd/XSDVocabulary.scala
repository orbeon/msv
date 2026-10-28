package org.orbeon.msv.reader.datatype.xsd

import org.orbeon.msv.datatype.xsd.DatatypeFactory
import org.orbeon.msv.reader.State
import org.orbeon.msv.reader.datatype.DataTypeVocabulary
import org.orbeon.msv.relaxng.datatype.{Datatype, DatatypeException}
import org.orbeon.msv.util.StartTagInfo

/**
 * XSD implementation of [[DataTypeVocabulary]].
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class XSDVocabulary extends DataTypeVocabulary with Serializable:

  def createTopLevelReaderState(tag: StartTagInfo): State =
    if tag.localName == "simpleType" then new SimpleTypeState()
    else null

  def getType(localTypeName: String): Datatype =
    DatatypeFactory.getTypeByName(localTypeName)

object XSDVocabulary:
  val XMLSchemaNamespace: String = "http://www.w3.org/2001/XMLSchema-datatypes"
  val XMLSchemaNamespace2: String = "http://www.w3.org/2001/XMLSchema"

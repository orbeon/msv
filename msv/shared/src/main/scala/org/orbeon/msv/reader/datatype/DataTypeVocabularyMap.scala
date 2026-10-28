package org.orbeon.msv.reader.datatype

import java.util.Map as JMap
import org.orbeon.msv.reader.datatype.xsd.XSDVocabulary

/**
 * a map from namespace URI to DataTypeVocabulary
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class DataTypeVocabularyMap extends Serializable:

  /** map from namespace URI to DataTypeVocabulary */
  private val impl: JMap[String, DataTypeVocabulary] = new java.util.HashMap[String, DataTypeVocabulary]()

  def get(namespaceURI: String): DataTypeVocabulary =
    var v = impl.get(namespaceURI)
    if v != null then return v

    if namespaceURI == XSDVocabulary.XMLSchemaNamespace || namespaceURI == XSDVocabulary.XMLSchemaNamespace2 then
      v = new XSDVocabulary()
      impl.put(XSDVocabulary.XMLSchemaNamespace, v)
      impl.put(XSDVocabulary.XMLSchemaNamespace2, v)

    v

  def put(namespaceURI: String, voc: DataTypeVocabulary): Unit =
    impl.put(namespaceURI, voc)

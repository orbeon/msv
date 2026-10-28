package org.orbeon.msv.reader.datatype.xsd

/**
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait XSDatatypeResolver:
  def resolveXSDatatype(qName: String): XSDatatypeExp

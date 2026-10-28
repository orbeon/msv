package org.orbeon.msv.datatype.xsd.conformance

import org.orbeon.msv.datatype.SerializationContext
import org.orbeon.msv.relaxng.datatype.ValidationContext

/**
 * Test implementation of ValidationContext and SerializationContext.
 */
object DummyContextProvider extends ValidationContext with SerializationContext:

  override def resolveNamespacePrefix(prefix: String): String =
    prefix match
      case "foo"         => "http://foo.examples.com"
      case "bar" | "baz" => "http://bar.examples.com"
      case "" | "emp"    => "http://empty.examples.com"
      case _             => null

  override def getNamespacePrefix(uri: String): String =
    uri match
      case "http://foo.examples.com"   => "foo"
      case "http://bar.examples.com"   => "bar"
      case "http://empty.examples.com" => null
      case _                           => "xyz"

  override def isUnparsedEntity(name: String): Boolean =
    name == "foo" || name == "bar"

  override def isNotation(name: String): Boolean =
    name == "foo" || name == "bar"

  override def getBaseUri: String = null

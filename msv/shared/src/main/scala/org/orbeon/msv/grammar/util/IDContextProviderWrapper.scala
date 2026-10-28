package org.orbeon.msv.grammar.util

import org.orbeon.msv.grammar.{IDContextProvider, IDContextProvider2}
import org.orbeon.msv.relaxng.datatype.Datatype
import org.orbeon.msv.verifier.regexp.StringToken
import scala.annotation.nowarn

/**
 * Wraps [[IDContextProvider]] so that it can be used
 * where [[IDContextProvider2]] is expected.
 *
 * @author Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
@nowarn("cat=deprecation")
final class IDContextProviderWrapper private (private val core: IDContextProvider) extends IDContextProvider2 {
  def getBaseUri: String = core.getBaseUri

  def isNotation(arg0: String): Boolean = core.isNotation(arg0)

  def isUnparsedEntity(arg0: String): Boolean = core.isUnparsedEntity(arg0)

  def onID(datatype: Datatype, token: StringToken): Unit = {
    core.onID(datatype, token.literal)
  }

  def resolveNamespacePrefix(arg0: String): String = core.resolveNamespacePrefix(arg0)
}

@nowarn("cat=deprecation")
object IDContextProviderWrapper {
  def create(core: IDContextProvider): IDContextProvider2 = {
    if (core == null) null
    else new IDContextProviderWrapper(core)
  }
}

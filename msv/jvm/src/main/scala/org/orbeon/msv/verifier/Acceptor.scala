package org.orbeon.msv.verifier

import org.orbeon.msv.grammar.{IDContextProvider, IDContextProvider2}
import org.orbeon.msv.util.{DatatypeRef, StartTagInfo, StringRef}
import scala.annotation.static

/**
 * exposes the low-level validation engine interface.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait Acceptor:

  def createChildAcceptor(sti: StartTagInfo, refErr: StringRef): Acceptor

  def onAttribute2(
    namespaceURI: String,
    localName: String,
    qName: String,
    value: String,
    context: IDContextProvider2,
    refErr: StringRef,
    refType: DatatypeRef
  ): Boolean

  @deprecated("use onAttribute2")
  def onAttribute(
    namespaceURI: String,
    localName: String,
    qName: String,
    value: String,
    context: IDContextProvider,
    refErr: StringRef,
    refType: DatatypeRef
  ): Boolean

  def onEndAttributes(sti: StartTagInfo, refErr: StringRef): Boolean

  def onText2(literal: String, context: IDContextProvider2, refErr: StringRef, refType: DatatypeRef): Boolean

  @deprecated("use onText2")
  def onText(literal: String, context: IDContextProvider, refErr: StringRef, refType: DatatypeRef): Boolean

  def stepForward(child: Acceptor, errRef: StringRef): Boolean

  def isAcceptState(errRef: StringRef): Boolean

  def getOwnerType: AnyRef

  def createClone(): Acceptor

  def getStringCareLevel: Int

object Acceptor:
  @static final val STRING_PROHIBITED = 0x00
  @static final val STRING_IGNORE     = 0x01
  @static final val STRING_STRICT     = 0x02

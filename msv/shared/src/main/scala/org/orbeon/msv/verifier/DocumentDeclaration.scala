package org.orbeon.msv.verifier

/**
 * Represents a kind of "constraint" over XML document.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait DocumentDeclaration:
  def createAcceptor(): Acceptor

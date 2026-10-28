package org.orbeon.msv.grammar.xmlschema

/**
 * model group declaration.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
class GroupDeclExp(typeLocalName: String) extends RedefinableExp(typeLocalName):

  def getClone(): RedefinableExp =
    val exp = new GroupDeclExp(this.name)
    exp.redefine(this)
    exp

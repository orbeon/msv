package org.orbeon.msv.grammar.relax

/**
 * exportable Expression.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait Exportable:
  def isExported(): Boolean

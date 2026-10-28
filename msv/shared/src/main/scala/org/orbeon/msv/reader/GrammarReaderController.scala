package org.orbeon.msv.reader

import org.xml.sax.{EntityResolver, Locator}

/**
 * Event notification interface for controlling grammar parsing process.
 *
 *  1. receives notification of errors and warnings while parsing a grammar
 *  2. controls how inclusion of other grammars are processed (through [[EntityResolver]]).
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
trait GrammarReaderController extends EntityResolver:
  def warning(locs: Array[Locator], errorMessage: String): Unit
  def error(locs: Array[Locator], errorMessage: String, nestedException: Exception): Unit

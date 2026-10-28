package org.orbeon.msv.reader

/**
 * This exception will be thrown when the schema parsing is aborted
 * after all the errors are reported through GrammarReaderController.
 *
 * To throw this exception, the thrower must report an error to
 * the controller. This exception can be caught anytime by anyone
 * who can recover from the error.
 * The receiver shouldn't report this exception to the application,
 * since it has already been reported by the thrower.
 */
class AbortException private () extends Exception("aborted. Errors should have been reported")

object AbortException:
  val theInstance: AbortException = new AbortException()

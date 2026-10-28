package org.orbeon.msv.grammar.trex

/**
 * formats messages by using a resource file.
 *
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
private[trex] object Localizer:
  private val messages: Map[String, String] = Map(
    "TypedString.Diagnosis" -> "value must be \"{0}\""
  )

  def format(formatStr: String, args: Array[AnyRef]): String =
    if args == null || args.isEmpty then formatStr
    else
      var res = formatStr
      var i = 0
      while i < args.length do
        val s = if args(i) != null then args(i).toString else "null"
        res = res.replace("{" + i + "}", s)
        i += 1
      res

  def localize(prop: String, args: Array[AnyRef]): String =
    val template = messages.getOrElse(prop, prop)
    format(template, args)

  def localize(prop: String): String = localize(prop, null.asInstanceOf[Array[AnyRef]])
  def localize(prop: String, arg1: AnyRef): String = localize(prop, Array(arg1))

package org.orbeon.msv.datatype

import org.orbeon.msv.relaxng.datatype.{Datatype, ValidationContext}

/**
 * Datatype interface that supports Java databinding.
 *
 * This interface can be used to do java/xml databinding.
 * 
 * @author Kohsuke Kawaguchi
 */
trait DatabindableDatatype extends Datatype {

  /**
   * Converts lexical value to a corresponding Java-friendly object
   * by using the given context information.
   * 
   * @return
   *   null when the given lexical value is not a valid lexical value for this type.
   */
  def createJavaObject(literal: String, context: ValidationContext): AnyRef

  /**
   * Converts a value object back to the lexical representation.
   * 
   * <p>
   * This method is a kind of the "reverse" function of the createJavaObject method.
   * 
   * @param context
   *   The context object is used to obtain information necessary to
   *   serialize the value object. For example, QName type uses the context
   *   to encode the URI into a prefix.
   * 
   * @exception IllegalArgumentException
   *   If the type of the specified value object is not recognized,
   *   this exception is thrown. For example, if you pass
   *   a <code>String</code> object to the serializeJavaObject method of
   *   the "positiveInteger" type, this exception is thrown.
   * 
   * @return
   *   null if the given object is invalid with respect to this datatype.
   */
  @throws(classOf[IllegalArgumentException])
  def serializeJavaObject(value: AnyRef, context: SerializationContext): String

  /**
   * Gets the type of the objects that are created by the createJavaObject method.
   */
  def getJavaObjectType(): Class[?]
}

package org.orbeon.msv.relaxng.datatype

import utest.*
import org.orbeon.msv.relaxng.datatype.helpers.*

object RelaxngDatatypeTest extends TestSuite {

  val tests = Tests {
    test("DatatypeException") {
      val ex1 = new DatatypeException()
      assert(ex1.getIndex == DatatypeException.UNKNOWN)
      assert(ex1.getMessage == null)

      val ex2 = new DatatypeException("custom error")
      assert(ex2.getIndex == DatatypeException.UNKNOWN)
      assert(ex2.getMessage == "custom error")

      val ex3 = new DatatypeException(42, "error at 42")
      assert(ex3.getIndex == 42)
      assert(ex3.getMessage == "error at 42")
    }

    test("ParameterlessDatatypeBuilder") {
      val dummyDatatype = new Datatype {
        def isValid(literal: String, context: ValidationContext): Boolean = literal == "valid"
        def checkValid(literal: String, context: ValidationContext): Unit = {
          if (literal != "valid") throw new DatatypeException("invalid")
        }
        def createStreamingValidator(context: ValidationContext): DatatypeStreamingValidator =
          new StreamingValidatorImpl(this, context)
        def createValue(literal: String, context: ValidationContext): AnyRef = literal
        def sameValue(value1: AnyRef, value2: AnyRef): Boolean = value1 == value2
        def valueHashCode(value: AnyRef): Int = value.hashCode()
        def getIdType(): Int = Datatype.ID_TYPE_NULL
        def isContextDependent(): Boolean = false
      }

      val builder = new ParameterlessDatatypeBuilder(dummyDatatype)
      assert(builder.createDatatype() eq dummyDatatype)

      val caught = try {
        builder.addParameter("foo", "bar", null)
        false
      } catch {
        case _: DatatypeException => true
      }
      assert(caught)
    }

    test("StreamingValidatorImpl") {
      val dummyDatatype = new Datatype {
        def isValid(literal: String, context: ValidationContext): Boolean = literal == "hello world"
        def checkValid(literal: String, context: ValidationContext): Unit = {
          if (literal != "hello world") throw new DatatypeException("mismatch")
        }
        def createStreamingValidator(context: ValidationContext): DatatypeStreamingValidator =
          new StreamingValidatorImpl(this, context)
        def createValue(literal: String, context: ValidationContext): AnyRef = literal
        def sameValue(value1: AnyRef, value2: AnyRef): Boolean = value1 == value2
        def valueHashCode(value: AnyRef): Int = value.hashCode()
        def getIdType(): Int = Datatype.ID_TYPE_NULL
        def isContextDependent(): Boolean = false
      }

      val validator = dummyDatatype.createStreamingValidator(null)
      val part1 = "hello ".toCharArray
      val part2 = "world".toCharArray
      validator.addCharacters(part1, 0, part1.length)
      validator.addCharacters(part2, 0, part2.length)
      assert(validator.isValid)
      validator.checkValid()
    }

    test("DatatypeLibraryLoader registry") {
      val dummyFactory = new DatatypeLibraryFactory {
        def createDatatypeLibrary(namespaceURI: String): DatatypeLibrary =
          if (namespaceURI == "http://example.com/ns") new DatatypeLibrary {
            def createDatatypeBuilder(baseTypeLocalName: String): DatatypeBuilder = null
            def createDatatype(typeLocalName: String): Datatype = null
          }
          else null
      }

      DatatypeLibraryLoader.register(dummyFactory)
      val loader = new DatatypeLibraryLoader()
      val lib = loader.createDatatypeLibrary("http://example.com/ns")
      assert(lib != null)
      assert(loader.createDatatypeLibrary("http://nonexistent.org") == null)
    }
  }
}

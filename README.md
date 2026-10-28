# Orbeon MSV (Multi-Schema XML Validator)

A modern Scala 3 port of the Sun Multi-Schema XML Validator (MSV), cross-compiled for both the **Java Virtual Machine (JVM)** and **Scala.js** (browser and Node.js environments).

## Overview

The Sun Multi-Schema XML Validator (MSV) is a Java XML validation engine originally created by Kohsuke Kawaguchi at Sun Microsystems. MSV supports multiple schema languages and provides fast, streaming XML validation using a SAX-based pipeline.

This repository is **Orbeon's modernized fork**:
- **Scala 3 Native**: Completely ported from Java to Scala 3.
- **Cross-Platform**: Cross-compiled with [Scala.js](https://www.scala-js.org/) and JVM, enabling XML validation directly in web browsers and Node.js with zero JVM runtime dependencies.
- **Unified Library**: Unifies MSV core, Sun XML Datatypes Library (XSDLib), and the RELAX NG Datatypes interface into a single, cohesive project.
- **Production Proven**: Powers XML Schema and RELAX NG validation in [Orbeon Forms](https://www.orbeon.com/).
- **Conformant & Extensively Tested**: Verified against official W3C XML Schema Datatype conformance suites, RELAX NG test suites, and complex real-world industry schemas (OASIS UBL 2.3, UN/CEFACT CII, OASIS OpenDocument 1.3).

---

## Supported Schema Languages

MSV supports an extensive range of XML schema grammars:

| Schema Language | Specification / Details | Status |
| :--- | :--- | :--- |
| **W3C XML Schema 1.0 (XSD)** | XML Schema Part 1: Structures & Part 2: Datatypes | Full support for elements, complex/simple types, model groups (`sequence`, `choice`, `all`), wildcards (`any`, `anyAttribute`), substitution groups, identity constraints (`xs:unique`, `xs:key`, `xs:keyref`), facets, and all 44+ built-in datatypes |
| **RELAX NG** | OASIS RELAX NG 1.0 (XML Syntax) | Full support including `interleave`, `except` name classes, definitions with `combine`, `externalRef`, `include`, and datatypes |
| **RELAX NG DTD Compatibility** | OASIS RELAX NG DTD Compatibility | Default attribute values, ID/IDREF/IDREFS referential integrity, and documentation |
| **TREX** | Tree Regular Expressions for XML | Full support (James Clark's predecessor to RELAX NG) |
| **RELAX Core & RELAX Namespace** | JIS:TR X 0029:2000 | Supported |
| **XML DTD** | XML 1.0 Recommendation | Element declarations, attribute lists, entity declarations |
| **ISO RELAX / JARV** | Java API for RELAX Verifiers | Supported |

---

## Getting Started

### Using with sbt

Artifacts are published to **GitHub Packages**.

Add the resolver and dependency to your `build.sbt`:

```scala
resolvers += Resolver.githubPackages("orbeon")

// For Scala.js or cross-projects:
libraryDependencies += "org.orbeon" %%% "msv" % "2026.1-SNAPSHOT"

// For JVM-only projects:
libraryDependencies += "org.orbeon" %% "msv" % "2026.1-SNAPSHOT"
```

---

## Usage Examples

### 1. Validating XML against a W3C XML Schema (XSD)

```scala
import org.orbeon.msv.grammar.xmlschema.XMLSchemaGrammar
import org.orbeon.msv.reader.xmlschema.XMLSchemaReader
import org.orbeon.msv.reader.GrammarReaderController
import org.orbeon.msv.verifier.identity.IDConstraintChecker
import org.xml.sax.{ErrorHandler, InputSource, Locator, SAXParseException}
import javax.xml.parsers.SAXParserFactory
import java.io.StringReader

// Define a controller for schema warnings/errors and entity resolution
class SchemaController extends GrammarReaderController:
  def warning(locs: Array[Locator], msg: String): Unit = println(s"Schema warning: $msg")
  def error(locs: Array[Locator], msg: String, e: Exception): Unit = println(s"Schema error: $msg")
  def resolveEntity(publicId: String, systemId: String): InputSource = null

// Define an error handler for validation diagnostics
class ValidationErrorHandler extends ErrorHandler:
  var errors: List[String] = Nil
  def warning(e: SAXParseException): Unit = ()
  def error(e: SAXParseException): Unit = errors = e.getMessage :: errors
  def fatalError(e: SAXParseException): Unit = errors = e.getMessage :: errors

// 1. Prepare SAX parser factory
val factory = SAXParserFactory.newInstance()
factory.setNamespaceAware(true)

// 2. Parse the schema into an XMLSchemaGrammar
val schemaSource = new InputSource(new StringReader(schemaXml))
schemaSource.setSystemId("http://example.com/schema.xsd")
val grammar: XMLSchemaGrammar = XMLSchemaReader.parse(schemaSource, factory, new SchemaController)

// 3. Validate an XML document instance
val errorHandler = new ValidationErrorHandler
val verifier = new IDConstraintChecker(grammar, errorHandler)

val parser = factory.newSAXParser()
val xmlReader = parser.getXMLReader
xmlReader.setContentHandler(verifier)
xmlReader.setErrorHandler(errorHandler)

xmlReader.parse(new InputSource(new StringReader(instanceXml)))

if verifier.isValid && errorHandler.errors.isEmpty then
  println("Document is valid!")
else
  println(s"Document is invalid. Errors: ${errorHandler.errors}")
```

### 2. Validating XML against a RELAX NG Schema

```scala
import org.orbeon.msv.reader.trex.ng.RELAXNGReader
import org.orbeon.msv.verifier.Verifier
import org.xml.sax.InputSource
import javax.xml.parsers.SAXParserFactory
import java.io.StringReader

val factory = SAXParserFactory.newInstance()
factory.setNamespaceAware(true)

val rngSource = new InputSource(new StringReader(relaxNgXml))
val grammar = RELAXNGReader.parse(rngSource, factory, new SchemaController)

val errorHandler = new ValidationErrorHandler
val verifier = new Verifier(grammar.getTopLevel, errorHandler)

val xmlReader = factory.newSAXParser().getXMLReader
xmlReader.setContentHandler(verifier)
xmlReader.setErrorHandler(errorHandler)

xmlReader.parse(new InputSource(new StringReader(instanceXml)))

val isValid = verifier.isValid && errorHandler.errors.isEmpty
```

### 3. Validating Built-in XML Schema Datatypes Directly

MSV bundles the complete W3C XML Schema Datatypes engine:

```scala
import org.orbeon.msv.datatype.xsd.DatatypeFactory

val decimalType = DatatypeFactory.getTypeByName("decimal")
assert(decimalType.isValid("123.45", null))
assert(!decimalType.isValid("abc", null))

val dateTimeType = DatatypeFactory.getTypeByName("dateTime")
assert(dateTimeType.isValid("2026-10-28T12:00:00Z", null))
assert(!dateTimeType.isValid("2026-13-45T99:99:99", null))
```

---

## Building and Testing

Prerequisites:
- JDK 17+ (or JDK 11+)
- [sbt](https://www.scala-sbt.org/) 1.9+
- [Node.js](https://nodejs.org/) (for running Scala.js test suites)

```bash
# Compile both JVM and Scala.js targets
sbt compile

# Run the complete test suite across JVM and Scala.js
sbt test

# Run JVM tests only (including large real-world schema tests)
sbt msvJVM/test

# Run Scala.js tests only
sbt msvJS/test

# Publish snapshot to local Ivy repository
sbt publishLocal
```

---

## Project Structure

```
.
├── build.sbt                 # Cross-platform sbt build definition
├── README.md                 # Top-level documentation
├── LICENSE.txt               # Main MSV BSD-style license
├── Apache-LICENSE-1.1.txt    # Apache Software License 1.1
├── RELAXNG-LICENSE.txt       # RELAX NG Datatypes BSD-style license
├── msv/
│   ├── shared/               # Cross-compiled Scala 3 source and tests
│   │   ├── src/main/scala/   # MSV, XSDLib, and RELAX NG datatypes
│   │   └── src/test/         # Shared test suites (XSD, RNG, datatypes)
│   ├── jvm/                  # JVM-specific integration tests & large schemas
│   └── js/                   # Scala.js target configuration
```

---

## History and Credits

- **Original Creator**: Kohsuke Kawaguchi at Sun Microsystems.
- **Upstream Development**: Maintained as part of Sun/Oracle open source XML initiatives and upstream community forks such as [xmlark/msv](https://github.com/xmlark/msv).
- **Orbeon Fork**: Ported to Scala 3, cross-compiled for Scala.js, and maintained by [Orbeon](https://www.orbeon.com/).

---

## License

This software is distributed under open-source licenses:

- **MSV Core & XSDLib**: Sun Microsystems BSD-style License — see [LICENSE.txt](LICENSE.txt).
- **Apache-derived Components** (regular expressions in datatypes): The Apache Software License, Version 1.1 — see [Apache-LICENSE-1.1.txt](Apache-LICENSE-1.1.txt).
- **RELAX NG Datatypes**: Thai Open Source Software Center Ltd & Sun Microsystems BSD License — see [RELAXNG-LICENSE.txt](RELAXNG-LICENSE.txt).

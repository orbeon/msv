/*
 * @(#)$Id: DefaultDatatypeLibraryFactory.java,v 1.2 2003/06/09 20:37:31 kk122374 Exp $
 *
 * Copyright 2002 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.reader.trex.ng;

import org.relaxng.datatype.DatatypeLibrary;
import org.relaxng.datatype.DatatypeLibraryFactory;

import com.sun.msv.grammar.relaxng.datatype.BuiltinDatatypeLibrary;
import com.sun.msv.grammar.relaxng.datatype.CompatibilityDatatypeLibrary;
import com.sun.msv.reader.datatype.xsd.XSDVocabulary;

import java.util.Enumeration;
import java.util.Vector;
import java.util.NoSuchElementException;
import java.net.URL;
import java.io.*;

/**
 * Default implementation of Datatype
 * 
 * @author
 *     Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
class DefaultDatatypeLibraryFactory implements DatatypeLibraryFactory {

    // ORBEON: Use custom loader as standard DatatypeLibraryLoader is broken in JDK 1.6.0 on OS X
    private final DatatypeLibraryFactory loader = new CustomDatatypeLibraryLoader();
    
    private DatatypeLibrary xsdlib;
    
    private DatatypeLibrary compatibilityLib;

    /**
     * @see org.relaxng.datatype.DatatypeLibraryFactory#createDatatypeLibrary(java.lang.String)
     */
    public DatatypeLibrary createDatatypeLibrary(String namespaceURI) {
        
        DatatypeLibrary lib = loader.createDatatypeLibrary(namespaceURI);
        if(lib!=null)       return lib;
        
        // if failed to dynamically locate the library, use static ones.
        
        if( namespaceURI.equals("") )
            return BuiltinDatatypeLibrary.theInstance;
        
        // We have the built-in support for XML Schema Part 2.
        if( namespaceURI.equals(XSDVocabulary.XMLSchemaNamespace)
        ||  namespaceURI.equals(XSDVocabulary.XMLSchemaNamespace2) ) {
            if(xsdlib==null)
                xsdlib = new com.sun.msv.datatype.xsd.ngimpl.DataTypeLibraryImpl();
            return xsdlib;
        }
        
        // RELAX NG compatibiltiy datatypes library is also supported
        if( namespaceURI.equals(CompatibilityDatatypeLibrary.namespaceURI) ) {
            if( compatibilityLib==null )
                compatibilityLib = new CompatibilityDatatypeLibrary();
            return compatibilityLib;
        }
        
        return null;
    }
    
}

/**
 * Copyright (c) 2001, Thai Open Source Software Center Ltd
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *     Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 *     Neither the name of the Thai Open Source Software Center Ltd nor
 *     the names of its contributors may be used to endorse or promote
 *     products derived from this software without specific prior written
 *     permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * Discovers the datatype library implementation from the classpath.
 *
 *
 * The call of the createDatatypeLibrary method finds an implementation
 * from a given datatype library URI at run-time.
 */
class CustomDatatypeLibraryLoader implements DatatypeLibraryFactory {
    private final Service service = new Service(DatatypeLibraryFactory.class);

    public DatatypeLibrary createDatatypeLibrary(String uri) {
        for (Enumeration e = service.getProviders();
             e.hasMoreElements();) {
            DatatypeLibraryFactory factory
                    = (DatatypeLibraryFactory) e.nextElement();
            DatatypeLibrary library = factory.createDatatypeLibrary(uri);
            if (library != null)
                return library;
        }
        return null;
    }

    private static class Service {
        private final Class serviceClass;
        private final Enumeration configFiles;
        private Enumeration classNames = null;
        private final Vector providers = new Vector();
        private Loader loader;

        private class ProviderEnumeration implements Enumeration {
            private int nextIndex = 0;

            public boolean hasMoreElements() {
                return nextIndex < providers.size() || moreProviders();
            }

            public Object nextElement() {
                try {
                    return providers.elementAt(nextIndex++);
                }
                catch (ArrayIndexOutOfBoundsException e) {
                    throw new NoSuchElementException();
                }
            }
        }

        private static class Singleton implements Enumeration {
            private Object obj;

            private Singleton(Object obj) {
                this.obj = obj;
            }

            public boolean hasMoreElements() {
                return obj != null;
            }

            public Object nextElement() {
                if (obj == null)
                    throw new NoSuchElementException();
                Object tem = obj;
                obj = null;
                return tem;
            }
        }

        // JDK 1.1
        private static class Loader {
            Enumeration getResources(String resName) {
                ClassLoader cl = Loader.class.getClassLoader();
                URL url;
                if (cl == null)
                    url = ClassLoader.getSystemResource(resName);
                else
                    url = cl.getResource(resName);
                return new Singleton(url);
            }

            Class loadClass(String name) throws ClassNotFoundException {
                return Class.forName(name);
            }
        }

        // JDK 1.2+
        private static class Loader2 extends Loader {
            private ClassLoader cl;

            Loader2() {
                // ORBEON: This can return null if these classes are returned by the bootstrap class loader
                cl = Loader2.class.getClassLoader();
                // If the thread context class loader has the class loader
                // of this class as an ancestor, use the thread context class
                // loader.  Otherwise, the thread context class loader
                // probably hasn't been set up properly, so don't use it.
                ClassLoader clt = Thread.currentThread().getContextClassLoader();
                for (ClassLoader tem = clt; tem != null; tem = tem.getParent())
                    if (tem == cl) {
                        cl = clt;
                        break;
                    }
            }

            Enumeration getResources(String resName) {
                // ORBEON: cl cann be null if these classes are returned by the bootstrap class loader
                if (cl == null)
                    return super.getResources(resName);

                try {
                    return cl.getResources(resName);
                } catch (IOException e) {
                    return new Singleton(null);
                }
            }

            Class loadClass(String name) throws ClassNotFoundException {
                // ORBEON: cl cann be null if these classes are returned by the bootstrap class loader
                if (cl == null)
                    return super.loadClass(name);
                return Class.forName(name, true, cl);
            }
        }

        public Service(Class cls) {
            try {
                loader = new Loader2();
            } catch (NoSuchMethodError e) {
                loader = new Loader();
            }
            serviceClass = cls;
            String resName = "META-INF/services/" + serviceClass.getName();
            configFiles = loader.getResources(resName);
        }

        public Enumeration getProviders() {
            return new ProviderEnumeration();
        }

        synchronized private boolean moreProviders() {
            for (; ;) {
                while (classNames == null) {
                    if (!configFiles.hasMoreElements())
                        return false;
                    classNames = parseConfigFile((URL) configFiles.nextElement());
                }
                while (classNames.hasMoreElements()) {
                    String className = (String) classNames.nextElement();
                    try {
                        Class cls = loader.loadClass(className);
                        Object obj = cls.newInstance();
                        if (serviceClass.isInstance(obj)) {
                            providers.addElement(obj);
                            return true;
                        }
                    }
                    catch (ClassNotFoundException e) {
                    }
                    catch (InstantiationException e) {
                    }
                    catch (IllegalAccessException e) {
                    }
                    catch (LinkageError e) {
                    }
                }
                classNames = null;
            }
        }

        private static final int START = 0;
        private static final int IN_NAME = 1;
        private static final int IN_COMMENT = 2;

        private static Enumeration parseConfigFile(URL url) {
            try {
                InputStream in = url.openStream();
                Reader r;
                try {
                    r = new InputStreamReader(in, "UTF-8");
                }
                catch (UnsupportedEncodingException e) {
                    r = new InputStreamReader(in, "UTF8");
                }
                r = new BufferedReader(r);
                Vector tokens = new Vector();
                StringBuffer tokenBuf = new StringBuffer();
                int state = START;
                for (; ;) {
                    int n = r.read();
                    if (n < 0)
                        break;
                    char c = (char) n;
                    switch (c) {
                        case '\r':
                        case '\n':
                            state = START;
                            break;
                        case ' ':
                        case '\t':
                            break;
                        case '#':
                            state = IN_COMMENT;
                            break;
                        default:
                            if (state != IN_COMMENT) {
                                state = IN_NAME;
                                tokenBuf.append(c);
                            }
                            break;
                    }
                    if (tokenBuf.length() != 0 && state != IN_NAME) {
                        tokens.addElement(tokenBuf.toString());
                        tokenBuf.setLength(0);
                    }
                }
                if (tokenBuf.length() != 0)
                    tokens.addElement(tokenBuf.toString());
                return tokens.elements();
            }
            catch (IOException e) {
                return null;
            }
        }
    }
}

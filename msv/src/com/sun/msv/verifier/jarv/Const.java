/*
 * @(#)$Id: Const.java,v 1.1 2001/11/19 19:57:35 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.verifier.jarv;

/**
 * Constant values related to JARV.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public class Const
{
    /**
     * Turns on/off the panic mode.
     * <p>
     * Panic mode is on by default. Turning it on is good
     * for general use. It prevents false error messages to appear.
     * <p>
     * However, turning it off is sometimes useful, when
     * you are sure that the structure of the document is
     * almost valid (e.g., validating a document generated by an
     * application or when you are sure about the validity of the
     * structure and only interested in validating datatypes,)
     */
    public static final String PANIC_MODE_FEATURE =
        "http://www.sun.com/xmlns/msv/features/panicMode";
}

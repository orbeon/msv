/*
 * @(#)$Id: Debug.java,v 1.6 2003/06/09 20:37:16 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.driver.textui;

public class Debug
{
    public static boolean debug;
    
    static {
        try {
            debug = System.getProperty("com.sun.msv.debug")!=null;
        } catch( SecurityException e ) {
            // a security manager might reject this call
            debug = false;
        }
    }
}

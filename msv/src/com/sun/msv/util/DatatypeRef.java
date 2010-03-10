/*
 * @(#)$Id: DatatypeRef.java,v 1.11 2003/06/09 20:37:40 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.util;

/**
 * reference to {@link org.relaxng.datatype.Datatype}
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public class DatatypeRef {
    /**
     * if the size of the array is zero, then that means this token is ignored.
     */
    public org.relaxng.datatype.Datatype[] types = null;
}

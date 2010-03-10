/*
 * @(#)$Id: AttWildcardExp.java,v 1.2 2003/06/09 20:37:22 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.grammar.xmlschema;

/**
 * Schema component with {@link AttributeWildcard}
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
public interface AttWildcardExp {
    /**
     * Gets the attribute wildcard property of this schema component.
     * 
     * @return
     *        If the value is absent, null is returned.
     */
    AttributeWildcard getAttributeWildcard();
}

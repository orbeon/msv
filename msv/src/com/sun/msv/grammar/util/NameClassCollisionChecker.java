/*
 * @(#)$Id: NameClassCollisionChecker.java,v 1.5 2004/04/05 17:42:20 kohsuke Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.grammar.util;

/**
 * Computes if two name classes collide or not.
 * 
 * <p>
 * This comparator returns true if the intersection of two name classes
 * is non empty.
 * 
 * <p>
 * The same thing can be computed by using the
 * {@link com.sun.msv.grammar.NameClass#intersection(NameClass, NameClass)} method,
 * but generally this method is faster.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
public class NameClassCollisionChecker extends NameClassComparator {
            
    protected void probe( String uri, String local ) {
        if(nc1.accepts(uri,local) && nc2.accepts(uri,local))
            // conflict is found.
            throw eureka;
    }
}
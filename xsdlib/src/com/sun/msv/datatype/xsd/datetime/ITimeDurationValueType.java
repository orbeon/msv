/*
 * @(#)$Id: ITimeDurationValueType.java,v 1.10 2003/06/09 20:49:25 kk122374 Exp $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.datatype.xsd.datetime;

/**
 * interface as a value type of TimeDurationType
 * 
 * @author Kohsuke KAWAGUCHI
 */
public interface ITimeDurationValueType extends java.io.Serializable {
    BigTimeDurationValueType getBigValue();

    /** compare two ITimeDurationValueType as defined in
     *  com.sun.msv.datatype/Comparator
     */
    int compare( ITimeDurationValueType rhs );
}

package com.sun.msv.datatype.xsd.regex;

import com.sun.msv.datatype.regexp.InternalImpl;

import java.text.ParseException;

// Just return the internal implementation, as that's the one that would be found anyway
public abstract class RegExpFactory {
    public abstract RegExp compile( String exp ) throws ParseException ;

    public static RegExpFactory createFactory() {
        return new InternalImpl();
    }
}

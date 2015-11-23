/*
 * Copyright (c) 2012, 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * The Universal Permissive License (UPL), Version 1.0
 *
 * Subject to the condition set forth below, permission is hereby granted to any
 * person obtaining a copy of this software, associated documentation and/or
 * data (collectively the "Software"), free of charge and under any and all
 * copyright rights in the Software, and any and all patent rights owned or
 * freely licensable by each licensor hereunder covering either (i) the
 * unmodified Software as contributed to or provided by such licensor, or (ii)
 * the Larger Works (as defined below), to deal in both
 *
 * (a) the Software, and
 *
 * (b) any piece of software and/or hardware listed in the lrgrwrks.txt file if
 * one is included with the Software each a "Larger Work" to which the Software
 * is contributed by such licensors),
 *
 * without restriction, including without limitation the rights to copy, create
 * derivative works of, display, perform, and distribute the Software and make,
 * use, sell, offer for sale, import, export, have made, and have sold the
 * Software and the Larger Work(s), and to sublicense the foregoing rights on
 * either these or other terms.
 *
 * This license is subject to the following condition:
 *
 * The above copyright notice and either this complete permission notice or at a
 * minimum a reference to the UPL must be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.oracle.truffle.erl.runtime.misc;

import java.util.Calendar;
import java.util.TimeZone;

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlTuple;

public final class Time {

    public static final TimeZone LOCAL_TIMEZONE = TimeZone.getDefault();
    public static final TimeZone UTC_TIMEZONE = TimeZone.getTimeZone("UTC");

    public static Calendar parseDateTimeTuple(ErlTuple pair) {
        return parseDateTimeTuple(pair, UTC_TIMEZONE);
    }

    public static Calendar parseDateTimeTuple(ErlTuple pair, TimeZone timezone) {
        if (2 == pair.getSize()) {

            final ErlTuple date = ErlTuple.fromObject(pair.getElement(1));
            final ErlTuple time = ErlTuple.fromObject(pair.getElement(2));

            if (3 == date.getSize() && 3 == time.getSize()) {

                Calendar cal = Calendar.getInstance(timezone);
                cal.set(ErlContext.decodeInt(date.getElement(1)), ErlContext.decodeInt(date.getElement(2)) - 1, ErlContext.decodeInt(date.getElement(3)), ErlContext.decodeInt(time.getElement(1)),
                                ErlContext.decodeInt(time.getElement(2)), ErlContext.decodeInt(time.getElement(3)));

                return cal;
            }
        }

        throw ErlControlException.makeBadarg();
    }

    public static ErlTuple calendarToDate(Calendar cal) {
        return new ErlTuple((long) cal.get(Calendar.YEAR), (long) cal.get(Calendar.MONTH) + 1, (long) cal.get(Calendar.DAY_OF_MONTH));
    }

    public static ErlTuple calendarToTime(Calendar cal) {
        return new ErlTuple((long) cal.get(Calendar.HOUR_OF_DAY), (long) cal.get(Calendar.MINUTE), (long) cal.get(Calendar.SECOND));
    }

    public static ErlTuple calendarToDateTime(Calendar cal) {
        return new ErlTuple(calendarToDate(cal), calendarToTime(cal));
    }
}

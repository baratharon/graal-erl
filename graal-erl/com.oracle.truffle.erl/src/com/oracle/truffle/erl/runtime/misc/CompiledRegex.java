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

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.ErlAtom;
import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlTuple;

/**
 * Compiled regular expression.
 * <p>
 * The Erlang representation:
 * <code>{re_pattern, SequenceNumber, JavaHashCodeOfTheRegexString, RegexStringAsUtf8Binary, OriginalOptions}</code>
 *
 */
public final class CompiledRegex {

    private static long nextSequenceNumber = 1;
    private static final int MAXIMUM_NUMBER_OF_CACHED_REGEX = 16;
    private static final LRUCache<Long, CompiledRegex> cache = new LRUCache<>(MAXIMUM_NUMBER_OF_CACHED_REGEX);

    private final long seqNum;
    private final Pattern pattern;
    private final ErlList options;
    private final RegexOptions ro;
    private ErlTuple cachedTuple = null;

    private static synchronized long generateSequenceNumber() {
        return nextSequenceNumber++;
    }

    private static synchronized void cacheRegex(CompiledRegex cre) {
        cache.put(cre.seqNum, cre);
    }

    private CompiledRegex(Pattern pattern, final ErlList options, final RegexOptions ro) {
        this.seqNum = generateSequenceNumber();
        this.pattern = pattern;
        this.options = options;
        this.ro = ro;
    }

    public static CompiledRegex compile(Object iodata, ErlList options) {
        final CompiledRegex cre = doCompile(iodata, options, RegexOptions.parse(options, true));
        cacheRegex(cre);
        return cre;
    }

    private static CompiledRegex doCompile(Object iodata, ErlList options, final RegexOptions ro) {

        final byte[] bytes = IoList.toArray(iodata);
        final String regex = new String(bytes);

        int flags = 0;

        if (ro.dotall()) {
            flags |= Pattern.DOTALL;
        }

        return new CompiledRegex(Pattern.compile(regex, flags), options, ro);
    }

    public static CompiledRegex lookup(ErlTuple tuple) {

        CompiledRegex cre;

        if (isValidMPTuple(tuple)) {
            throw ErlControlException.makeBadarg();
        }

        synchronized (CompiledRegex.class) {
            cre = cache.get(tuple.getElement(2));
        }

        if (null == cre || cre.pattern.hashCode() != (long) tuple.getElement(3)) {
            // TODO: sequence number and hash code is enough?
            cre = compile(tuple.getElement(4), (ErlList) tuple.getElement(5));
        }

        return cre;
    }

    private static CompiledRegex toCompiledRegex(Object re, ErlList options, RegexOptions ro) {

        if ((re instanceof ErlTuple) && isValidMPTuple((ErlTuple) re)) {
            CompiledRegex cre = lookup((ErlTuple) re);
            ro.update(cre.options, false).update(options, false);
            return cre;
        }

        return doCompile(re, options, ro);
    }

    public static Object run(Object subject, Object re, ErlList options) {

        final RegexOptions ro = RegexOptions.parse(options, false);
        final CompiledRegex cre = toCompiledRegex(re, options, ro);
        final String subjectString = new String(IoList.toArray(subject));

        Matcher m = cre.pattern.matcher(subjectString);

        if (m.find()) {

            final ArrayList<Object> matches = new ArrayList<>();

            do {

                final CaptureType ct = ro.captureType();

                switch (ct) {
                    case INDEX: {
                        final int start = m.start();
                        matches.add(new ErlTuple((long) start, (long) (m.end() - start)));
                        break;
                    }

                    case LIST: {
                        final String data = m.group();
                        matches.add(ErlList.fromString(data));
                        break;
                    }

                    case BINARY: {
                        final String data = m.group();
                        matches.add(ErlBinary.fromString(data, ro.unicode() ? ErlContext.UTF8_CHARSET : ErlContext.LATIN1_CHARSET));
                        break;
                    }
                }
            } while (ro.global() && m.find());

            return new ErlTuple(ErlAtom.MATCH, ErlList.fromList(matches));

        } else {
            return ErlAtom.NOMATCH;
        }
    }

    /**
     * Opaque datatype containing a compiled regular expression. The mp() is guaranteed to be a
     * tuple() having the atom 're_pattern' as its first element, to allow for matching in guards.
     * The arity of the tuple() or the content of the other fields may change in future releases.
     */
    private static boolean isValidMPTuple(ErlTuple tuple) {
        return (5 == tuple.getSize()) && (ErlAtom.RE_PATTERN.equals(tuple.getElement(1))) && (tuple.getElement(2) instanceof Long) && (tuple.getElement(3) instanceof Long) &&
                        (tuple.getElement(4) instanceof ErlBinary) && (tuple.getElement(5) instanceof ErlList);
    }

    public ErlTuple toTuple() {

        if (null == cachedTuple) {
            cachedTuple = new ErlTuple(ErlAtom.RE_PATTERN, seqNum, (long) pattern.pattern().hashCode(), ErlBinary.fromString(pattern.pattern(), ErlContext.UTF8_CHARSET), options);
        }

        assert isValidMPTuple(cachedTuple);

        return cachedTuple;
    }
}

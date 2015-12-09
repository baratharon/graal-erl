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
package com.oracle.truffle.erl.runtime;

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.util.Comparator;
import java.util.TreeMap;
import java.util.function.BiConsumer;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.misc.ExternalTerm;

/**
 * The Erlang tuples are represented as array of objects.
 */
public final class ErlMap implements TruffleObject {

    public final static class Assoc {

        final Object key;
        final Object value;
        final boolean exact;

        public Assoc(Object key, Object value) {
            this.key = key;
            this.value = value;
            this.exact = false;
        }

        public Assoc(Object key, Object value, boolean exact) {
            this.key = key;
            this.value = value;
            this.exact = exact;
        }

        public Object getKey() {
            return key;
        }

        public Object getValue() {
            return value;
        }

        public boolean isExact() {
            return exact;
        }
    }

    private static final class KeyComparator implements Comparator<Object> {

        public int compare(Object lhs, Object rhs) {

            // In maps key order integers types are considered less than floats types.

            if ((lhs instanceof Long || lhs instanceof BigInteger) && lhs instanceof Double) {
                return -1;
            } else if (lhs instanceof Double && (lhs instanceof Long || lhs instanceof BigInteger)) {
                return +1;
            }

            return ErlContext.compareTerms(lhs, rhs, true);
        }
    }

    final private TreeMap<Object, Object> mapping;

    private static final KeyComparator KEY_COMPARATOR = new KeyComparator();
    public static final ErlMap EMPTY = ErlMap.fromArray(new Assoc[]{});

    @TruffleBoundary
    public ErlMap(Assoc... elements) {

        assert null != elements;
        this.mapping = new TreeMap<>(KEY_COMPARATOR);

        for (Assoc element : elements) {
            assert false == element.exact;
            mapping.put(element.key, element.value);
        }
    }

    private ErlMap() {

        this.mapping = new TreeMap<>(KEY_COMPARATOR);
    }

    public static ErlMap fromArray(Assoc elements[]) {
        return new ErlMap(elements);
    }

    private final static class ToStringConsumer implements BiConsumer<Object, Object> {

        private StringBuilder sb;
        private String sep;

        private ToStringConsumer(StringBuilder sb) {
            this.sb = sb;
            this.sep = "";
        }

        @TruffleBoundary
        public void accept(Object key, Object value) {

            sb.append(sep);
            sb.append(key);
            sb.append(" => ");
            sb.append(value);
            sep = ",";
        }
    }

    @TruffleBoundary
    public ErlMap makeUpdated(Assoc... assocs) {

        ErlMap result = new ErlMap();
        result.mapping.putAll(mapping);

        for (Assoc assoc : assocs) {

            if (assoc.exact && !result.mapping.containsKey(assoc.key)) {
                throw ErlControlException.makeBadkey(assoc.key);
            }

            result.mapping.put(assoc.key, assoc.value);
        }

        return result;
    }

    @TruffleBoundary
    public ErlMap makeRemoved(Object... keys) {

        ErlMap result = new ErlMap();
        result.mapping.putAll(mapping);

        for (Object key : keys) {

            result.mapping.remove(key);
        }

        return result;
    }

    @TruffleBoundary
    public Object find(Object key) {
        return mapping.get(key);
    }

    @TruffleBoundary
    public boolean isKey(Object key) {
        return null != mapping.get(key);
    }

    @TruffleBoundary
    public Object[] getKeyArray() {
        return mapping.keySet().toArray();
    }

    @TruffleBoundary
    public Object[] getValueArray() {
        return mapping.values().toArray();
    }

    @Override
    @TruffleBoundary
    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("#{");
        mapping.forEach(new ToStringConsumer(sb));
        sb.append('}');

        return sb.toString();
    }

    @TruffleBoundary
    public int getSize() {
        return mapping.size();
    }

    @TruffleBoundary
    public int compare(ErlMap rhs, boolean exact) {

        final ErlMap lhs = this;

        // Maps are ordered by size, two maps with the same size are compared by keys in ascending
        // term order and then by values in key order. In maps key order integers types are
        // considered less than floats types.

        if (lhs.getSize() < rhs.getSize()) {
            return -1;
        } else if (lhs.getSize() > rhs.getSize()) {
            return +1;
        } else {
            final Object[] lhsKeys = lhs.mapping.keySet().toArray();
            final Object[] rhsKeys = rhs.mapping.keySet().toArray();

            assert lhsKeys.length == rhsKeys.length;

            for (int i = 0; i < lhsKeys.length; ++i) {

                final int cmp = ErlContext.compareTerms(lhsKeys[i], rhsKeys[i], true);

                if (0 != cmp) {
                    return cmp;
                }
            }
        }

        final Object[] lhsValues = lhs.mapping.values().toArray();
        final Object[] rhsValues = rhs.mapping.values().toArray();

        assert lhsValues.length == rhsValues.length;

        for (int i = 0; i < lhsValues.length; ++i) {

            final int cmp = ErlContext.compareTerms(lhsValues[i], rhsValues[i], exact);

            if (0 != cmp) {
                return cmp;
            }
        }

        return 0;
    }

    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }

    private final static class EncoderConsumer implements BiConsumer<Object, Object> {

        private final ExternalTerm.Encoder encoder;
        private boolean ok = true;

        public EncoderConsumer(ExternalTerm.Encoder encoder) {
            this.encoder = encoder;
        }

        @TruffleBoundary
        public void accept(Object key, Object value) {
            ok = ok && encoder.encode(key);
            ok = ok && encoder.encode(value);
        }
    }

    @TruffleBoundary
    public boolean encode(ByteArrayOutputStream out, ExternalTerm.Encoder encoder) {

        out.write(ExternalTerm.MAP_EXT);
        out.write((mapping.size() >>> 24) & 0xff);
        out.write((mapping.size() >>> 16) & 0xff);
        out.write((mapping.size() >>> 8) & 0xff);
        out.write(mapping.size() & 0xff);

        EncoderConsumer ec = new EncoderConsumer(encoder);
        mapping.forEach(ec);
        return ec.ok;
    }
}

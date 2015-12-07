/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
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
package com.oracle.truffle.erl.parser;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

class StackableSet<T> {

    public static interface Monitor<T> {
        public void add(T element);

        public void access(T element);
    }

    public static class CollectAccessOf<T> implements Monitor<T> {
        public final HashSet<T> access = new HashSet<>();
        public final Set<T> refSet;

        public CollectAccessOf(Set<T> refSet) {
            this.refSet = refSet;
        }

        public void add(T element) {
            // not interested
        }

        public void access(T element) {
            if (refSet.contains(element)) {
                access.add(element);
            }
        }
    }

    private final LinkedList<HashSet<T>> stack = new LinkedList<>();
    private final LinkedList<Integer> acceptAllStack = new LinkedList<>();
    private HashSet<T> current = new HashSet<>();
    private int acceptAll = 0;
    private final LinkedList<Monitor<T>> monitors = new LinkedList<>();

    public StackableSet() {
        // nothing to do here
    }

    public void monitorAdd(Monitor<T> monitor) {
        monitors.add(monitor);
    }

    public void monitorRemove(Monitor<T> monitor) {
        monitors.remove(monitor);
    }

    public boolean add(T element) {

        final boolean result = current.add(element);

        if (0 < acceptAll) {
            for (Monitor<T> m : monitors) {
                m.access(element);
            }
            return false;
        }

        if (result) {
            for (Monitor<T> m : monitors) {
                m.add(element);
            }
        } else {
            for (Monitor<T> m : monitors) {
                m.access(element);
            }
        }

        return result;
    }

    public void addAll(Collection<? extends T> collection) {
        current.addAll(collection);
    }

    public void push() {
        stack.add(new HashSet<>(current));
    }

    public void pop() {
        current = stack.getLast();
        stack.removeLast();
    }

    public Set<T> getChangeSet() {
        HashSet<T> result = new HashSet<>(current);
        result.removeAll(stack.getLast());
        return result;
    }

    public Set<T> get() {
        return new HashSet<>(current);
    }

    public void beginAcceptAll() {
        ++acceptAll;
    }

    public void endAcceptAll() {
        assert 0 < acceptAll;
        --acceptAll;
    }

    public void pushAcceptAll() {
        acceptAllStack.push(acceptAll);
        acceptAll = 0;
    }

    public void popAcceptAll() {
        acceptAll = acceptAllStack.pop();
    }
}

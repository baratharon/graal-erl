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
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.HashSet;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.ForeignAccess;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.runtime.misc.ExternalTerm;

/**
 * The Erlang atoms are represented as strings.
 */
public final class ErlAtom implements TruffleObject {

    private final String value;
    private final int hvalue;
    private final boolean isRegularAtom;

    public static final ErlAtom TRUE = new ErlAtom("true");
    public static final ErlAtom FALSE = new ErlAtom("false");
    public static final ErlAtom VALUE = new ErlAtom("value");
    public static final ErlAtom _DOWN = new ErlAtom("DOWN");
    public static final ErlAtom _EXIT = new ErlAtom("EXIT");
    public static final ErlAtom EXIT = new ErlAtom("exit");
    public static final ErlAtom CLOSE = new ErlAtom("close");
    public static final ErlAtom CLOSED = new ErlAtom("closed");
    public static final ErlAtom UNDEFINED = new ErlAtom("undefined");
    public static final ErlAtom TIMEOUT = new ErlAtom("timeout");
    public static final ErlAtom TIMEOUT_VALUE = new ErlAtom("timeout_value");
    public static final ErlAtom SYSTEM_LIMIT = new ErlAtom("system_limit");
    public static final ErlAtom OK = new ErlAtom("ok");
    public static final ErlAtom NORMAL = new ErlAtom("normal");
    public static final ErlAtom LOW = new ErlAtom("low");
    public static final ErlAtom HIGH = new ErlAtom("high");
    public static final ErlAtom MAX = new ErlAtom("max");
    public static final ErlAtom ERROR = new ErlAtom("error");
    public static final ErlAtom WARNING = new ErlAtom("warning");
    public static final ErlAtom THROW = new ErlAtom("throw");
    public static final ErlAtom BADMATCH = new ErlAtom("badmatch");
    public static final ErlAtom UNDEF = new ErlAtom("undef");
    public static final ErlAtom BAD_GENERATOR = new ErlAtom("bad_generator");
    public static final ErlAtom BAD_FILTER = new ErlAtom("bad_filter");
    public static final ErlAtom BADKEY = new ErlAtom("badkey");
    public static final ErlAtom BADMAP = new ErlAtom("badmap");
    public static final ErlAtom PROCESS = new ErlAtom("process");
    public static final ErlAtom NOPROC = new ErlAtom("noproc");
    public static final ErlAtom TRAP_EXIT = new ErlAtom("trap_exit");
    public static final ErlAtom MONITOR_NODES = new ErlAtom("monitor_nodes");
    public static final ErlAtom PRIORITY = new ErlAtom("priority");
    public static final ErlAtom FLUSH = new ErlAtom("flush");
    public static final ErlAtom SPAWN = new ErlAtom("spawn");
    public static final ErlAtom SPAWN_DRIVER = new ErlAtom("spawn_driver");
    public static final ErlAtom SPAWN_EXECUTABLE = new ErlAtom("spawn_executable");
    public static final ErlAtom FD = new ErlAtom("fd");
    public static final ErlAtom INFO = new ErlAtom("info");
    public static final ErlAtom INFINITY = new ErlAtom("infinity");
    public static final ErlAtom BIG = new ErlAtom("big");
    public static final ErlAtom LITTLE = new ErlAtom("little");
    public static final ErlAtom LATIN1 = new ErlAtom("latin1");
    public static final ErlAtom UNICODE = new ErlAtom("unicode");
    public static final ErlAtom UTF8 = new ErlAtom("utf8");
    public static final ErlAtom UTF16 = new ErlAtom("utf16");
    public static final ErlAtom UTF32 = new ErlAtom("utf32");
    public static final ErlAtom DATA = new ErlAtom("data");
    public static final ErlAtom COMPRESSED = new ErlAtom("compressed");
    public static final ErlAtom MINOR_VERSION = new ErlAtom("minor_version");
    public static final ErlAtom NONODE_NOHOST = new ErlAtom("nonode@nohost");
    public static final ErlAtom NOTSUP = new ErlAtom("notsup");
    public static final ErlAtom THREADS = new ErlAtom("threads");
    public static final ErlAtom THREAD_POOL_SIZE = new ErlAtom("thread_pool_size");
    public static final ErlAtom PURIFY = new ErlAtom("purify");
    public static final ErlAtom OS_TYPE = new ErlAtom("os_type");
    public static final ErlAtom WIN32 = new ErlAtom("win32");
    public static final ErlAtom NT = new ErlAtom("nt");
    public static final ErlAtom UNIX = new ErlAtom("unix");
    public static final ErlAtom LINUX = new ErlAtom("linux");
    public static final ErlAtom MAC = new ErlAtom("mac");
    public static final ErlAtom HIPE_ARCHITECTURE = new ErlAtom("hipe_architecture");
    public static final ErlAtom REGISTERED_NAME = new ErlAtom("registered_name");
    public static final ErlAtom LINK = new ErlAtom("link");
    public static final ErlAtom MONITOR = new ErlAtom("monitor");
    public static final ErlAtom FULLSWEEP_AFTER = new ErlAtom("fullsweep_after");
    public static final ErlAtom MIN_HEAP_SIZE = new ErlAtom("min_heap_size");
    public static final ErlAtom MIN_BIN_VHEAP_SIZE = new ErlAtom("min_bin_vheap_size");
    public static final ErlAtom NOSUSPEND = new ErlAtom("nosuspend");
    public static final ErlAtom NOCONNECT = new ErlAtom("noconnect");
    public static final ErlAtom SET = new ErlAtom("set");
    public static final ErlAtom ORDERED_SET = new ErlAtom("ordered_set");
    public static final ErlAtom BAG = new ErlAtom("bag");
    public static final ErlAtom DUPLICATE_BAG = new ErlAtom("duplicate_bag");
    public static final ErlAtom PRIVATE = new ErlAtom("private");
    public static final ErlAtom PROTECTED = new ErlAtom("protected");
    public static final ErlAtom PUBLIC = new ErlAtom("public");
    public static final ErlAtom HEIR = new ErlAtom("heir");
    public static final ErlAtom NONE = new ErlAtom("none");
    public static final ErlAtom NAMED_TABLE = new ErlAtom("named_table");
    public static final ErlAtom KEYPOS = new ErlAtom("keypos");
    public static final ErlAtom WRITE_CONCURRENCY = new ErlAtom("write_concurrency");
    public static final ErlAtom READ_CONCURRENCY = new ErlAtom("read_concurrency");
    public static final ErlAtom _ETS_TRANSFER = new ErlAtom("ETS-TRANSFER");
    public static final ErlAtom _END_OF_TABLE = new ErlAtom("$end_of_table");
    public static final ErlAtom MODULE = new ErlAtom("module");
    public static final ErlAtom FUNCTIONS = new ErlAtom("functions");
    public static final ErlAtom EXPORTS = new ErlAtom("exports");
    public static final ErlAtom MD5 = new ErlAtom("md5");
    public static final ErlAtom ATTRIBUTES = new ErlAtom("attributes");
    public static final ErlAtom COMPILE = new ErlAtom("compile");
    public static final ErlAtom NATIVE = new ErlAtom("native");
    public static final ErlAtom DICTIONARY = new ErlAtom("dictionary");
    public static final ErlAtom MESSAGES = new ErlAtom("messages");
    public static final ErlAtom LINKS = new ErlAtom("links");
    public static final ErlAtom STATUS = new ErlAtom("status");
    public static final ErlAtom HEAP_SIZE = new ErlAtom("heap_size");
    public static final ErlAtom GROUP_LEADER = new ErlAtom("group_leader");
    public static final ErlAtom STACK_SIZE = new ErlAtom("stack_size");
    public static final ErlAtom REDUCTIONS = new ErlAtom("reductions");
    public static final ErlAtom RUNNING = new ErlAtom("running");
    public static final ErlAtom RUNNABLE = new ErlAtom("runnable");
    public static final ErlAtom BINARY = new ErlAtom("binary");
    public static final ErlAtom INDEX = new ErlAtom("index");
    public static final ErlAtom LIST = new ErlAtom("list");
    public static final ErlAtom CONNECTED = new ErlAtom("connected");
    public static final ErlAtom ID = new ErlAtom("id");
    public static final ErlAtom OS_PID = new ErlAtom("os_pid");
    public static final ErlAtom INPUT = new ErlAtom("input");
    public static final ErlAtom OUTPUT = new ErlAtom("output");
    public static final ErlAtom IN = new ErlAtom("in");
    public static final ErlAtom OUT = new ErlAtom("out");
    public static final ErlAtom EOF = new ErlAtom("eof");
    public static final ErlAtom KILL = new ErlAtom("kill");
    public static final ErlAtom KILLED = new ErlAtom("killed");
    public static final ErlAtom COMMAND = new ErlAtom("command");
    public static final ErlAtom CONNECT = new ErlAtom("connect");
    public static final ErlAtom ARITY = new ErlAtom("arity");
    public static final ErlAtom ENV = new ErlAtom("env");
    public static final ErlAtom UNIQ = new ErlAtom("uniq");
    public static final ErlAtom NEW_UNIQ = new ErlAtom("new_uniq");
    public static final ErlAtom NEW_INDEX = new ErlAtom("new_index");
    public static final ErlAtom NAME = new ErlAtom("name");
    public static final ErlAtom PID = new ErlAtom("pid");
    public static final ErlAtom TYPE = new ErlAtom("type");
    public static final ErlAtom VERSION = new ErlAtom("version");
    public static final ErlAtom ALL = new ErlAtom("all");
    public static final ErlAtom ALL_NAMES = new ErlAtom("all_names");
    public static final ErlAtom ALL_BUT_FIRST = new ErlAtom("all_but_first");
    public static final ErlAtom FIRST = new ErlAtom("first");
    public static final ErlAtom LAST = new ErlAtom("last");
    public static final ErlAtom COMPACT = new ErlAtom("compact");
    public static final ErlAtom SCIENTIFIC = new ErlAtom("scientific");
    public static final ErlAtom DECIMALS = new ErlAtom("decimals");
    public static final ErlAtom RE_PATTERN = new ErlAtom("re_pattern");
    public static final ErlAtom CR = new ErlAtom("cr");
    public static final ErlAtom LF = new ErlAtom("lf");
    public static final ErlAtom CRLF = new ErlAtom("crlf");
    public static final ErlAtom ANYCRLF = new ErlAtom("anycrlf");
    public static final ErlAtom ANY = new ErlAtom("any");
    public static final ErlAtom MATCH = new ErlAtom("match");
    public static final ErlAtom NOMATCH = new ErlAtom("nomatch");
    public static final ErlAtom ANCHORED = new ErlAtom("anchored");
    public static final ErlAtom GLOBAL = new ErlAtom("global");
    public static final ErlAtom LOCAL = new ErlAtom("local");
    public static final ErlAtom EXTERNAL = new ErlAtom("external");
    public static final ErlAtom BADARG = new ErlAtom("badarg");
    public static final ErlAtom BADARITH = new ErlAtom("badarith");
    public static final ErlAtom BADARITY = new ErlAtom("badarity");
    public static final ErlAtom BADFUN = new ErlAtom("badfun");
    public static final ErlAtom NOCATCH = new ErlAtom("nocatch");
    public static final ErlAtom FUNCTION_CLAUSE = new ErlAtom("function_clause");
    public static final ErlAtom IF_CLAUSE = new ErlAtom("if_clause");
    public static final ErlAtom CASE_CLAUSE = new ErlAtom("case_clause");
    public static final ErlAtom TRY_CLAUSE = new ErlAtom("try_clause");

    public static ErlAtom fromBoolean(boolean value) {
        if (value) {
            return TRUE;
        }

        return FALSE;
    }

    public static ErlAtom fromObject(Object obj) {

        if (obj instanceof ErlAtom) {
            return (ErlAtom) obj;
        }

        if (obj instanceof Boolean) {
            return fromBoolean((boolean) obj);
        }

        throw ErlControlException.makeBadarg();
    }

    @TruffleBoundary
    public ErlAtom(String value) {
        this.value = value;
        this.hvalue = calcHashValue(value);
        this.isRegularAtom = value.matches("[a-z][a-zA-Z0-9@_]*") && !ErlContext.isKeyword(value);
    }

    public String getValue() {

        // TODO: remove it and replace with a string->int mapping?
        boolean TODO_remove = true;

        return value;
    }

    @Override
    public String toString() {
        if (isRegularAtom) {
            return value;
        }

        return escapeAtom(value);
    }

    @TruffleBoundary
    public static String escapeAtom(String str) {

        StringBuilder sb = new StringBuilder();
        sb.append('\'');

        for (int i = 0, n = str.length(); i < n; ++i) {

            final char ch = str.charAt(i);

            if (noGlyph.contains(ch)) {

                String res = "";
                int num = ch;

                while (num != 0) {
                    res = ((char) ('0' + (num & 0x7))) + res;
                    num >>= 3;
                }

                while (res.length() < 3) {
                    res = "0" + res;
                }

                sb.append("\\" + res);

            } else if (replace.containsKey(ch)) {

                sb.append(replace.get(ch));

            } else {
                sb.append(ch);
            }
        }

        return sb.append('\'').toString();
    }

    private static int decodeHexChar(char ch) {

        if ('0' <= ch && ch <= '9') {
            return ch - '0';
        }

        if ('a' <= ch && ch <= 'f') {
            return ch - 'a' + 10;
        }

        if ('A' <= ch && ch <= 'F') {
            return ch - 'A' + 10;
        }

        throw new RuntimeException("This is not a hexadecimal character: \'" + ch + "\'");
    }

    @TruffleBoundary
    public static String unescapeAtom(String str) {

        if ('\'' != str.charAt(0)) {
            return str;
        }

        if ('\'' != str.charAt(str.length() - 1)) {
            throw new RuntimeException("Badly formatted atom: \"" + str + "\"");
        }

        StringBuilder sb = new StringBuilder();

        for (int i = 1, n = str.length() - 1; i < n; ++i) {

            final char ch = str.charAt(i);

            if ('\\' == ch) {

                char recons = str.charAt(++i);

                switch (recons) {
                    case 'x': {
                        final char hi = str.charAt(++i);
                        final char lo = str.charAt(++i);
                        recons = (char) ((decodeHexChar(hi) << 4) | decodeHexChar(lo));
                        break;
                    }

                    case '0':
                    case '1': {
                        final char hi = recons;
                        final char mi = str.charAt(++i);
                        final char lo = str.charAt(++i);
                        recons = (char) (((hi - '0') * 8 + (mi - '0') * 8) + lo - '0');
                        break;
                    }
                }

                final String s = "\\" + recons;
                if (replaceInv.containsKey(s)) {
                    sb.append(replaceInv.get(s));
                } else if (noGlyph.contains(recons)) {
                    sb.append(recons);
                } else {
                    throw new RuntimeException("Cannot unescape atom string: \"" + str + "\"");
                }

            } else {
                sb.append(ch);
            }
        }

        return sb.toString();

    }

    @TruffleBoundary
    public static int compare(Object lhs, Object rhs) {

        if (lhs == rhs) {
            // very fast: compare by reference
            return 0;
        }

        // Boolean values are atoms, so we need to convert them.

        ErlAtom lhsAtom = (lhs instanceof ErlAtom) ? ((ErlAtom) lhs) : ErlAtom.fromBoolean((boolean) lhs);
        ErlAtom rhsAtom = (rhs instanceof ErlAtom) ? ((ErlAtom) rhs) : ErlAtom.fromBoolean((boolean) rhs);

        return lhsAtom.getValue().compareTo(rhsAtom.getValue());
    }

    @Override
    public int hashCode() {
        return hvalue;
    }

    @TruffleBoundary
    private boolean valueEquals(ErlAtom rhs) {
        return value.equals(rhs.value);
    }

    @Override
    public boolean equals(Object rhs) {

        if (this == rhs) {
            return true;
        }

        if (rhs instanceof ErlAtom) {

            // not equal atoms are filtered out quickly
            if (hvalue != ((ErlAtom) rhs).hvalue) {
                return false;
            }

            return valueEquals((ErlAtom) rhs);
        }

        if (rhs instanceof Boolean) {
            if ((boolean) rhs) {
                return TRUE.equals(this);
            }

            return FALSE.equals(this);
        }

        return false;
    }

    @TruffleBoundary
    public static int calcHashValue(String str) {
        long h = 0, g;

        for (int i = 0, n = str.length(); i < n; ++i) {
            h = (h << 4) + str.charAt(i);

            if (0 != (g = h & 0xf0000000)) {
                h ^= (g >> 24);
                h ^= g;
            }
        }

        return (int) h;
    }

    @Override
    public ForeignAccess getForeignAccess() {
        return ErlFunctionForeignAccess.create();
    }

    private static final HashSet<Character> noGlyph = new HashSet<>();
    private static final HashMap<Character, String> replace = new HashMap<>();
    private static final HashMap<String, Character> replaceInv = new HashMap<>();

    static {
        noGlyph.add('\000');
        noGlyph.add('\001');
        noGlyph.add('\002');
        noGlyph.add('\003');
        noGlyph.add('\004');
        noGlyph.add('\005');
        noGlyph.add('\006');
        noGlyph.add('\007');
        noGlyph.add('\016');
        noGlyph.add('\017');
        noGlyph.add('\020');
        noGlyph.add('\021');
        noGlyph.add('\022');
        noGlyph.add('\023');
        noGlyph.add('\024');
        noGlyph.add('\025');
        noGlyph.add('\026');
        noGlyph.add('\027');
        noGlyph.add('\030');
        noGlyph.add('\031');
        noGlyph.add('\032');
        noGlyph.add('\034');
        noGlyph.add('\035');
        noGlyph.add('\036');
        noGlyph.add('\037');

        replace.put('\b', "\\b");
        replace.put('\t', "\\t");
        replace.put('\n', "\\n");
        replace.put((char) 0xb, "\\v");
        replace.put('\f', "\\f");
        replace.put('\r', "\\r");
        replace.put((char) 033, "\\e");
        replace.put('\'', "\\'");
        replace.put('\\', "\\\\");

        replaceInv.put("\\b", '\b');
        replaceInv.put("\\t", '\t');
        replaceInv.put("\\n", '\n');
        replaceInv.put("\\v", (char) 0xb);
        replaceInv.put("\\f", '\f');
        replaceInv.put("\\r", '\r');
        replaceInv.put("\\e", (char) 033);
        replaceInv.put("\\'", '\'');
        replaceInv.put("\\\\", '\\');
    }

    public boolean encode(ByteArrayOutputStream out) {

        if (value.length() >= 0x10000) {
            return false;
        }

        if (isRegularAtom && value.length() <= 255) {

            out.write(ExternalTerm.ATOM_EXT);
            out.write(0);
            out.write(value.length());

            for (int i = 0, n = value.length(); i < n; ++i) {
                out.write(value.charAt(i));
            }

            return true;
        }

        ByteBuffer buf = ErlContext.UTF8_CHARSET.encode(value);

        out.write(ExternalTerm.ATOM_UTF8_EXT);
        out.write(buf.limit() >>> 8);
        out.write(buf.limit() & 0xff);

        while (buf.hasRemaining()) {
            out.write(buf.get());
        }

        return true;
    }
}

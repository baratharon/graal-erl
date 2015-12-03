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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.nio.file.Paths;

import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.erl.runtime.ErlModuleImpl;

/**
 * The Erlang parser.
 */
public class ErlParser {

    private static final String PARSER_PATH;

    static {

        String path = System.getenv("GRAAL_ERL_PARSER_PATH");

        if (null == path) {

            File file;

            // try to guess the path to the parser

            final String cwd = Paths.get("").toAbsolutePath().toString();

            for (String subdir : new String[]{".", "/erlang", "/../erlang", "/../../erlang"}) {

                path = cwd + subdir;
                file = new File(path);

                if (file.exists() && file.isDirectory()) {
                    break;
                }

                path = null;
            }

            if (null == path) {
                // no clue... use a dummy path, and let the erl to fail
                path = ".";
            }
        }

        PARSER_PATH = path;
    }

    public static ErlModuleImpl parseErlang(Source source) {
        Reader reader = source.getReader();

        try {

            // Preprocess the Erlang source with the Erlang parser,
            // which will export the AST into a temporary file.

            File sourceFile = File.createTempFile("erl_", ".erl");
            Writer writer = new BufferedWriter(new FileWriter(sourceFile));

            String moduleName = "";

            {
                String line = "";

                for (int ch = reader.read(); -1 != ch; ch = reader.read()) {
                    writer.write(ch);

                    if (moduleName.isEmpty()) {

                        // Small hack: we need the module name.
                        // It could be turned out by the filename, but we don't have any.

                        if ('.' == (char) ch) {

                            if (line.matches("-.*module.*[(].*[)].*")) {
                                String remain = line.substring(line.indexOf('(') + 1, line.lastIndexOf(')'));
                                moduleName = remain.trim();
                            }

                            line = "";
                        } else if ('\n' == (char) ch) {
                            line = "";
                        } else {
                            line = line + (char) ch;
                        }
                    }
                }
            }

            assert (!moduleName.isEmpty());

            reader.close();
            reader = null;

            writer.close();
            writer = null;

            File astFile = File.createTempFile("erl_", ".ast");
            int exitCode = Integer.MIN_VALUE;

            try {
                final String cmdLine = "erl -pa " + PARSER_PATH + " -run parser gen_ast " + moduleName + " " + astFile.getAbsolutePath() + " " + sourceFile.getAbsolutePath() +
                                " -run init stop -noshell";
                exitCode = Runtime.getRuntime().exec(cmdLine).waitFor();
            } catch (InterruptedException e) {
                sourceFile.delete();
                sourceFile = null;

                astFile.delete();
                astFile = null;

                exitCode = Integer.MIN_VALUE;
            }

            if (0 != exitCode) {
                throw new RuntimeException("Failed to parse Erlang source. Is \"erl\" accessable? Is \"parser.erl\" compiled and accessable?");
            }

            sourceFile.delete();
            sourceFile = null;

            // at this point, we have an alternate representation of the source as an AST
            final ErlModuleImpl module = parseErlangPreprocessed(false, moduleName, new FileReader(astFile));

            astFile.delete();
            astFile = null;

            return module;

        } catch (IOException ex) {
            ex.printStackTrace();
            throw new RuntimeException("I/O error during parsing Erlang source.");
        }
    }

    public static ErlModuleImpl parseErlangPreprocessed(final boolean preLoaded, final String moduleName, Reader reader) {

        ErlModuleImpl module = null;
        BufferedReader br = new BufferedReader(reader);
        try {

            String checkModuleName = br.readLine(); // first line contains the module name

            assert null == moduleName || moduleName.equals(checkModuleName);

            // the 'br' now points to the first character of the AST (which is actually an Erlang
            // term)
            try {
                module = new ErlModuleImpl(checkModuleName, preLoaded);
                new ErlAstParser(br, module).parse();
            } catch (RuntimeException ex) {
                ex.printStackTrace();
                throw ex;
            }

            br.close();
            br = null;

        } catch (IOException ex) {
            ex.printStackTrace();
            throw new RuntimeException("Failed to load preprocessed Erlang source.");
        }

        return module;
    }
}

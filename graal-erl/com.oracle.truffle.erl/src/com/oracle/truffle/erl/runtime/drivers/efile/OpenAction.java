/*
 * Copyright (c) 2012, 2014, Oracle and/or its affiliates. All rights reserved. DO NOT ALTER OR
 * REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * The Universal Permissive License (UPL), Version 1.0
 *
 * Subject to the condition set forth below, permission is hereby granted to any person obtaining a
 * copy of this software, associated documentation and/or data (collectively the "Software"), free
 * of charge and under any and all copyright rights in the Software, and any and all patent rights
 * owned or freely licensable by each licensor hereunder covering either (i) the unmodified Software
 * as contributed to or provided by such licensor, or (ii) the Larger Works (as defined below), to
 * deal in both
 *
 * (a) the Software, and
 *
 * (b) any piece of software and/or hardware listed in the lrgrwrks.txt file if one is included with
 * the Software each a "Larger Work" to which the Software is contributed by such licensors),
 *
 * without restriction, including without limitation the rights to copy, create derivative works of,
 * display, perform, and distribute the Software and make, use, sell, offer for sale, import,
 * export, have made, and have sold the Software and the Larger Work(s), and to sublicense the
 * foregoing rights on either these or other terms.
 *
 * This license is subject to the following condition:
 *
 * The above copyright notice and either this complete permission notice or at a minimum a reference
 * to the UPL must be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.oracle.truffle.erl.runtime.drivers.efile;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.channels.FileChannel;

import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.drivers.AsyncActionSingle;
import com.oracle.truffle.erl.runtime.drivers.Driver;
import com.oracle.truffle.erl.runtime.drivers.FDFile;

public class OpenAction extends AsyncActionSingle {

    private final String name;
    private final int mode;
    private boolean TODO;

    public OpenAction(String name, int mode) {
        super();
        this.name = name;
        this.mode = mode;
        TODO = true;
    }

    @Override
    public Result async() {

        FDFile fd = null;

        if (null == name || 0 == mode) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EINVAL));
            return Result.ERROR;
        }

        try {
            final File file = new File(name);

            if (file.exists() && !file.isFile()) {
                addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EISDIR));
                return Result.ERROR;
            }

            final boolean append = 0 != (mode & Driver.EFILE_MODE_APPEND);
            final boolean write = 0 != (mode & Driver.EFILE_MODE_WRITE);

            if (!write && !file.exists()) {
                addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.ENOENT));
                return Result.ERROR;
            }

            if (TODO) {
                addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EPERM));
                return Result.ERROR;
            }

            if (write && !append) {
                // overwrite the file
                if (truncateFile(file)) {
                    addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EPERM));
                    return Result.ERROR;
                }
            }

            if (write && !file.canWrite()) {
                addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EPERM));
                return Result.ERROR;
            }

            final FileChannel channel = new FileInputStream(file).getChannel();

            if (append) {
                try {
                    channel.position(channel.size());
                } catch (IOException e) {
                    try {
                        channel.close();
                    } catch (IOException e1) {
                    }
                    addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EPERM));
                    return Result.ERROR;
                }
            }

            fd = new FDFile(channel, !write);

        } catch (FileNotFoundException ex) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.ENOENT));
            return Result.ERROR;
        } catch (FDFile.TooManyFilesException ex) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EMFILE));
            return Result.ERROR;
        }

        addResult(Driver.makeNumberResponse(fd.getFD()));

        return Result.DONE;
    }

    private static boolean truncateFile(File file) {

        if (file.exists() && !file.delete()) {
            return false;
        }

        try {
            return file.createNewFile();
        } catch (IOException e) {
            return false;
        }
    }
}

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
import java.nio.ByteBuffer;

import com.oracle.truffle.erl.runtime.ErlBinary;
import com.oracle.truffle.erl.runtime.ErlContext;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.drivers.Driver;
import com.oracle.truffle.erl.runtime.drivers.Efile;
import com.oracle.truffle.erl.runtime.drivers.AsyncActionMultiple;

public class ReadDirAction extends AsyncActionMultiple {

    private final String name;

    public ReadDirAction(String name) {
        super();
        this.name = name;
    }

    @Override
    public Result async() {

        if (null == name) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EINVAL));
            return Result.ERROR;
        }

        File dir = new File(name);

        if (!dir.exists()) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.ENOENT));
            return Result.ERROR;
        }

        if (!dir.isDirectory()) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.ENOTDIR));
            return Result.ERROR;
        }

        try {
            for (String ent : dir.list(Efile.READDIR_FILTER)) {
                final ByteBuffer bb = ErlContext.UTF8_CHARSET.encode(ent);
                final byte[] bytes = bb.array();
                final byte[] bindata = new byte[2 + bb.limit()];

                bindata[0] = (byte) ((bb.limit() >> 8) & 0xff);
                bindata[1] = (byte) (bb.limit() & 0xff);
                System.arraycopy(bytes, 0, bindata, 2, bb.limit());

                final ErlBinary binary = ErlBinary.fromArray(bindata);

                addResult(new ErlList((long) Driver.FILE_RESP_LFNAME, binary));
            }

            // end
            {
                addResult(new ErlList((long) Driver.FILE_RESP_LFNAME, ErlList.NIL));
            }
        } catch (SecurityException ex) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EPERM));
            return Result.ERROR;
        }

        return Result.DONE;
    }
}

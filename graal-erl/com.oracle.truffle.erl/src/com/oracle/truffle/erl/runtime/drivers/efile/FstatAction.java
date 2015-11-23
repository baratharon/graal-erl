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
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.drivers.AsyncActionSingle;
import com.oracle.truffle.erl.runtime.drivers.Driver;

public class FstatAction extends AsyncActionSingle {

    private final String name;
    private final boolean lstat;

    public FstatAction(String name, boolean lstat) {
        super();
        this.name = name;
        this.lstat = lstat;
    }

    @Override
    public Result async() {

        if (null == name) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.EINVAL));
            return Result.ERROR;
        }

        File file = new File(name);

        if (!file.exists()) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.ENOENT));
            return Result.ERROR;
        }

        PosixFileAttributes attrs;
        Path path = file.toPath();

        try {
            if (lstat) {
                attrs = Files.readAttributes(path, PosixFileAttributes.class, LinkOption.NOFOLLOW_LINKS);
            } else {
                attrs = Files.readAttributes(path, PosixFileAttributes.class);
            }
        } catch (IOException ex) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.ENOENT));
            return Result.ERROR;
        }

        int file_type;

        if (attrs.isDirectory()) {
            file_type = Driver.FT_DIRECTORY;
        } else if (attrs.isRegularFile()) {
            file_type = Driver.FT_REGULAR;
        } else if (attrs.isSymbolicLink()) {
            file_type = Driver.FT_SYMLINK;
        } else if (attrs.isOther()) {
            file_type = Driver.FT_OTHER;
        } else {
            file_type = Driver.FT_DEVICE;
        }

        ByteBuffer bb = ByteBuffer.allocate(1 + 8 + 4 + 3 * 8 + 8 * 4);

        bb.put(Driver.FILE_RESP_INFO);
        bb.putLong(attrs.size());
        bb.putInt(file_type);
        bb.putLong(attrs.lastAccessTime().to(TimeUnit.SECONDS));
        bb.putLong(attrs.lastModifiedTime().to(TimeUnit.SECONDS));
        bb.putLong(attrs.creationTime().to(TimeUnit.SECONDS));

        Map<String, Object> unix_attrs;

        try {
            unix_attrs = Files.readAttributes(path, "unix:mode,ino,uid,gid,nlink,dev,rdev", LinkOption.NOFOLLOW_LINKS);
        } catch (UnsupportedOperationException e) {
            unix_attrs = null;
        } catch (IOException e) {
            addResult(new ErlList((long) Driver.FILE_RESP_ERROR, Driver.ENOENT));
            return Result.ERROR;
        }

        if (null != unix_attrs && unix_attrs.containsKey("mode")) {

            putAttr(bb, unix_attrs.get("mode"), 0);
            putAttr(bb, unix_attrs.get("nlink"), 1);
            putAttr(bb, unix_attrs.get("dev"), 0);
            putAttr(bb, unix_attrs.get("rdev"), 0);
            putAttr(bb, unix_attrs.get("ino"), 0);
            putAttr(bb, unix_attrs.get("uid"), 0);
            putAttr(bb, unix_attrs.get("gid"), 0);

        } else {

            bb.putInt(decodeFileMode(file_type, attrs));
            bb.putInt(1); // nlink
            bb.putInt(0); // dev
            bb.putInt(0); // rdev
            bb.putInt(0); // ino
            bb.putInt(0); // uid
            bb.putInt(0); // gid
        }

        {
            int file_access = 0;

            if (Files.isReadable(path)) {
                file_access |= Driver.EFILE_MODE_READ;
            }
            if (Files.isWritable(path)) {
                file_access |= Driver.EFILE_MODE_WRITE;
            }

            bb.putInt(file_access);
        }

        addResult(ErlList.fromByteArray(bb.array()));
        return Result.DONE;
    }

    private static int decodeFileMode(int file_type, PosixFileAttributes attrs) {

        int file_mode = 0;

        for (PosixFilePermission p : attrs.permissions()) {
            switch (p) {
                case OTHERS_READ:
                    file_mode |= 0000004;
                    break;

                case OTHERS_WRITE:
                    file_mode |= 0000002;
                    break;

                case OTHERS_EXECUTE:
                    file_mode |= 0000001;
                    break;

                case GROUP_READ:
                    file_mode |= 0000040;
                    break;

                case GROUP_WRITE:
                    file_mode |= 0000020;
                    break;

                case GROUP_EXECUTE:
                    file_mode |= 0000010;
                    break;

                case OWNER_READ:
                    file_mode |= 0000400;
                    break;

                case OWNER_WRITE:
                    file_mode |= 0000200;
                    break;

                case OWNER_EXECUTE:
                    file_mode |= 0000100;
                    break;
            }
        }

        switch (file_type) {
            case Driver.FT_DIRECTORY:
                file_mode |= 0040000;
                break;
            case Driver.FT_REGULAR:
                file_mode |= 0100000;
                break;
            case Driver.FT_SYMLINK:
                file_mode |= 0120000;
                break;
        }

        return file_mode;
    }

    private static void putAttr(ByteBuffer bb, Object value, int defaultValue) {

        if (null != value && (value instanceof Number)) {
            bb.putInt(((Number) value).intValue());
        } else {
            bb.putInt(defaultValue);
        }
    }
}

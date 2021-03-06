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
package com.oracle.truffle.erl.builtins._module;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.source.SourceSection;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.builtins.ErlBuiltinNode;
import com.oracle.truffle.erl.runtime.ErlList;
import com.oracle.truffle.erl.runtime.ErlModuleImpl;
import com.oracle.truffle.erl.runtime.ErlTuple;
import com.oracle.truffle.erl.runtime.misc.ModuleInfoItem;

/**
 * The module_info/0 function in each module, returns a list of {Key,Value} tuples with information
 * about the module. Currently, the list contain tuples with the following Keys: module, attributes,
 * compile, exports, md5 and native. The order and number of tuples may change without prior notice.
 */
@NodeInfo(shortName = "moduleInfo")
public abstract class ModuleInfo0Builtin extends ErlBuiltinNode {

    private final ErlModuleImpl module;

    public ModuleInfo0Builtin(ErlModuleImpl module) {
        super(SourceSection.createUnavailable("Erlang builtin", "module_info"));
        this.module = module;
    }

    @Override
    public MFA getName() {
        return new MFA(module.getModuleName(), "module_info", 0);
    }

    @Specialization
    public ErlList moduleInfo() {

        ErlList result = ErlList.NIL;

        for (ModuleInfoItem item : ModuleInfoItem.values()) {
            result = new ErlList(new ErlTuple(item.atom, module.getInfo(item)), result);
        }

        return result;
    }
}

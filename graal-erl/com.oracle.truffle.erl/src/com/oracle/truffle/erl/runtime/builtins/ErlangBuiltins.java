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
package com.oracle.truffle.erl.runtime.builtins;

import com.oracle.truffle.erl.builtins.erlang.AbsBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Adler1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Adler2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.AdlerCombineBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.AppendElementBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Apply2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Apply3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.AtomToBinaryBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.AtomToListBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.BinaryToListBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.BinaryToTerm1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.BitSizeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.BumpReductionsBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ByteSizeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.CancelTimer1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Crc1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Crc2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.CrcCombineBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.DateBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.DeleteElementBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Demonitor1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Demonitor2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.DisplayBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.DtAppendVmTagDataBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.DtPrependVmTagDataBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.DtRestoreTagBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.DtSpreadTagBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ElementBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Erase0BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Erase1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Error1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Error2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Exit1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Exit2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.FloatBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.FunctionExportedBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.GarbageCollect0BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Get0BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Get1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.GetKeys0BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.GetKeys1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.GetStacktraceBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.GroupLeader0BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.GroupLeader2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Halt0BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Halt1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Halt2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.HashBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.HdBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.InsertElementBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IntegerToList1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IntegerToList2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IolistSizeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IolistToBinaryBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsAliveBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsAtomBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsBinaryBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsBitstringBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsBooleanBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsBuiltinBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsFloatBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsFunction1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsFunction2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsIntegerBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsListBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsMapBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsNumberBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsPidBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsPortBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsProcessAliveBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsRecord2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsRecord3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsReferenceBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.IsTupleBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.LengthBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.LinkBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ListToAtomBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ListToBinaryBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ListToInteger1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ListToInteger2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ListToTupleBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.LocaltimeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.LocaltimeToUniversaltimeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.MakeRefBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.MakeTuple2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.MakeTuple3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.MapSizeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.MaxBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Md5BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Md5FinalBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Md5InitBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Md5UpdateBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.MinBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ModuleLoadedBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.MonitorBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.NifError1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.NifError2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Node0BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Node1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.OpenPortBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PhashBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PortCloseBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PortCommand2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PortCommand3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PortControlBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PosixtimeToUniversaltimeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PreLoadedBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ProcessFlag2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ProcessFlag3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ProcessInfo2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ProcessesBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.PutBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.RegisterBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.RegisteredBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.RoundBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SelfBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Send2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Send3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SendAfter3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SetelementBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SizeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Spawn1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.Spawn3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SpawnLink1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SpawnLink3BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SpawnOpt2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SpawnOpt4BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.SystemInfoBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.TermToBinary1BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.TermToBinary2BuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.ThrowBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.TimeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.TlBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.TruncBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.TupleSizeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.TupleToListBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.UniversaltimeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.UniversaltimeToLocaltimeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.UniversaltimeToPosixtimeBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.UnlinkBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.UnregisterBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.WhereisBuiltinFactory;
import com.oracle.truffle.erl.builtins.erlang.YieldBuiltinFactory;
import com.oracle.truffle.erl.runtime.ErlContext;

public final class ErlangBuiltins {

    public static void install(ErlContext context, final boolean registerRootNodes) {
        context.installBuiltin(IsIntegerBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsNumberBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsFloatBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsAtomBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsBooleanBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsListBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsTupleBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsReferenceBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsFunction1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsFunction2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsMapBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsBinaryBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsBitstringBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsBuiltinBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsPidBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsPortBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsRecord2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsRecord3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsProcessAliveBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IsAliveBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ProcessInfo2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(MakeRefBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(LengthBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(HdBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(TlBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(MapSizeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(TupleSizeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ElementBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(MakeTuple2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(MakeTuple3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(MaxBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(MinBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Md5BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Md5InitBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Md5FinalBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Md5UpdateBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(HashBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PhashBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Error1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Error2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(NifError1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(NifError2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Exit1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Exit2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ThrowBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(FloatBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(RoundBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(TruncBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(AbsBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(AdlerCombineBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Adler1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Adler2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(CrcCombineBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Crc1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Crc2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(AppendElementBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(DeleteElementBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(InsertElementBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SetelementBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SizeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Apply2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Apply3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(DateBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(TimeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PosixtimeToUniversaltimeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(UniversaltimeToPosixtimeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(UniversaltimeToLocaltimeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(LocaltimeToUniversaltimeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(LocaltimeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(UniversaltimeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(BitSizeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ByteSizeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IolistSizeBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IolistToBinaryBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(TupleToListBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ListToTupleBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ListToBinaryBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ListToInteger1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ListToInteger2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IntegerToList1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(IntegerToList2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SelfBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Node0BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Node1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(YieldBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ProcessesBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(MonitorBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Demonitor1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Demonitor2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(LinkBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(UnlinkBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Spawn1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Spawn3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SpawnLink1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SpawnLink3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SpawnOpt2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SpawnOpt4BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(RegisteredBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(RegisterBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(UnregisterBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(WhereisBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ProcessFlag2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ProcessFlag3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(GroupLeader0BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(GroupLeader2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Send2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Send3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SendAfter3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(CancelTimer1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PutBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Get0BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Get1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(GetKeys0BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(GetKeys1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Erase0BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Erase1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Halt0BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Halt1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(Halt2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(BumpReductionsBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(AtomToListBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(AtomToBinaryBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ListToAtomBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(BinaryToListBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(FunctionExportedBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(DisplayBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(TermToBinary1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(TermToBinary2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(BinaryToTerm1BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(SystemInfoBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(GetStacktraceBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(GarbageCollect0BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(ModuleLoadedBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PreLoadedBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(DtSpreadTagBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(DtAppendVmTagDataBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(DtPrependVmTagDataBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(DtRestoreTagBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(OpenPortBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PortCommand2BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PortCommand3BuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PortControlBuiltinFactory.getInstance(), registerRootNodes);
        context.installBuiltin(PortCloseBuiltinFactory.getInstance(), registerRootNodes);
    }
}

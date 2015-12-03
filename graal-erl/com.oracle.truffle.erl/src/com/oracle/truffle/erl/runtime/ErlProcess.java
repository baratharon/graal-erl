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

import java.io.Closeable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.erl.MFA;
import com.oracle.truffle.erl.nodes.controlflow.ErlControlException;
import com.oracle.truffle.erl.nodes.controlflow.ErlExitProcessException;
import com.oracle.truffle.erl.nodes.controlflow.ErlTailCallException;
import com.oracle.truffle.erl.runtime.ets.ErlTable;
import com.oracle.truffle.erl.runtime.misc.ProcInfoItem;
import com.oracle.truffle.erl.runtime.misc.Registrable;

/**
 * The Erlang processes are represented as java Callable.
 */
public final class ErlProcess implements Callable<Object>, Registrable {

    public final static class ProcessManager implements Closeable {

        private final ExecutorService threadPool = Executors.newCachedThreadPool();
        // private final ExecutorService threadPool =
        // Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

        private final HashMap<ErlPid, ErlProcess> processes = new HashMap<>();
        private final HashMap<String, Registrable> registry = new HashMap<>();
        private final HashMap<ErlRef, TimerRef> timers = new HashMap<>();
        private final HashSet<ErlPort> ports = new HashSet<>();
        private final Object processCountLock = new Object();

        private static final ThreadLocal<ErlProcess> currentProcess = new ThreadLocal<>();

        public void waitForTerminateAll() {
            synchronized (processCountLock) {
                while (!processes.isEmpty()) {
                    try {
                        processCountLock.wait();
                    } catch (InterruptedException e) {
                    }
                }
            }
        }

        public void close() {
            close(null);
        }

        void close(final ErlProcess excludedProcess) {
            synchronized (processes) {

                processes.forEach(new BiConsumer<ErlPid, ErlProcess>() {

                    public void accept(ErlPid pid, ErlProcess proc) {
                        if (excludedProcess != proc) {
                            proc.future.cancel(true);
                        }
                    }
                });

                timers.forEach(new BiConsumer<ErlRef, TimerRef>() {

                    public void accept(ErlRef ref, TimerRef timer) {
                        timer.future.cancel(true);
                    }
                });

                final ArrayList<ErlPort> portList = new ArrayList<>(ports);
                for (ErlPort port : portList) {
                    port.closeSync();
                }

                processes.clear();
                timers.clear();
                registry.clear();
                ports.clear();
            }

            threadPool.shutdown();
        }

        public Future<Object> put(ErlProcess proc) {
            synchronized (processes) {
                processes.put(proc.pid, proc);
            }
            return threadPool.submit(proc);
        }

        public ErlProcess findProcess(ErlPid pid) {
            synchronized (processes) {
                return processes.get(pid);
            }
        }

        public Registrable findRegisteredName(String name) {
            synchronized (processes) {
                return registry.get(name);
            }
        }

        public <T> T findRegisteredName(Class<T> registrable, String name) {
            synchronized (processes) {
                Registrable obj = registry.get(name);
                if (null != obj && registrable.isInstance(obj)) {
                    return registrable.cast(obj);
                }
                return null;
            }
        }

        public Set<ErlPid> getAllPid() {
            synchronized (processes) {
                return Collections.unmodifiableSet(new HashSet<>(processes.keySet()));
            }
        }

        public static void processEntry(ErlProcess proc) {
            currentProcess.set(proc);
        }

        public void processExit(ErlProcess proc, Object exitReason) {
            currentProcess.set(null);

            // System.err.println("EXIT " + proc.pid);

            synchronized (processes) {
                processes.remove(proc.pid);

                if (null != proc.registeredName) {
                    registry.remove(proc.registeredName);
                }

                proc.monitors.forEach(new MonitorNotify(proc, exitReason));

                for (ErlProcess p : proc.links) {

                    // unlink the other side
                    p.links.remove(proc);

                    p.killProcess(proc.pid, exitReason);
                }

                for (ErlTable t : proc.tables) {
                    t.onProcessDied();
                }

                synchronized (processCountLock) {
                    processCountLock.notifyAll();
                }
            }
        }

        public static ErlProcess getCurrentProcess() {
            return currentProcess.get();
        }

        public boolean register(String name, Registrable reg) {

            if (null != reg.getRegisteredName()) {
                return false;
            }

            synchronized (processes) {
                if (registry.containsKey(name)) {
                    return false;
                }

                registry.put(name, reg);
                reg.setRegisteredName(name);
            }

            return true;
        }

        public boolean unregister(String name) {

            synchronized (processes) {

                Registrable reg = registry.get(name);

                if (null == reg) {
                    return false;
                }

                registry.remove(name);
                reg.onUnregistered();

                return true;
            }
        }

        public Set<String> getRegisteredNames() {
            synchronized (processes) {
                return Collections.unmodifiableSet(new HashSet<>(registry.keySet()));
            }
        }
    }

    private final static class MonitorNotify implements BiConsumer<ErlRef, ErlProcess> {

        private final ErlProcess sender;
        private final Object reason;

        public MonitorNotify(ErlProcess sender, Object exitReason) {

            Object newReason = ErlAtom.NOPROC;

            if (exitReason instanceof ErlTuple) {
                final ErlTuple tuple = (ErlTuple) exitReason;

                if (tuple.getSize() > 1) {
                    newReason = tuple.getElement(tuple.getSize());
                }
            }

            this.sender = sender;
            this.reason = newReason;
        }

        public void accept(ErlRef ref, ErlProcess proc) {
            proc.sendMonitorDown(ref, sender.pid, reason);
        }
    }

    private final class Message {
        boolean present;
        final Object term;

        Message(Object term) {
            this.present = true;
            this.term = term;
        }
    }

    private final ErlContext context;
    private final ErlPid pid;
    private final ErlFunction initialFunction;
    private final Object[] initialArguments;
    private final LinkedBlockingDeque<Object> messageQueueIn = new LinkedBlockingDeque<>();
    private final LinkedList<Message> messageQueue = new LinkedList<>();
    private final HashSet<ErlProcess> links = new HashSet<>();
    private final HashSet<ErlTable> tables = new HashSet<>();
    private final TreeMap<Object, Object> dictionary = new TreeMap<>(ErlContext.TERM_COMPARATOR_EXACT);
    private final TreeMap<ErlRef, ErlProcess> monitors = new TreeMap<>(ErlRef.COMPARATOR);
    private final TreeMap<ErlRef, ErlProcess> foreignMonitors = new TreeMap<>(ErlRef.COMPARATOR);
    private final Future<Object> future;
    private final ProcessManager processManager;
    private final LinkedList<MFA> callStack = new LinkedList<>();
    private ErlPid groupLeader;
    private String registeredName = null;
    private Object exitReason = ErlTuple.EXIT_NORMAL;
    private ErlAtom priority = ErlAtom.NORMAL;
    private boolean trapExit = false;
    private boolean monitorNodes = false;

    private ErlProcess(ErlContext context, ErlProcess linkWith, ErlRef[] monitorRef, ErlFunction function, Object... arguments) {
        this.context = context;
        this.pid = ErlPid.make();
        this.initialFunction = function;
        this.initialArguments = arguments;
        this.processManager = context.getProcessManager();

        if (null != linkWith) {
            link(linkWith);
        }

        if (null != monitorRef) {
            monitorRef[0] = monitor(this.pid);
        }

        ErlProcess curr = getCurrentProcess();

        if (null != curr) {
            setGroupLeader(curr.groupLeader);
        } else {
            setGroupLeader(this.pid);
        }

        this.future = processManager.put(this);
    }

    public static ErlProcess spawn(ErlContext context, ErlFunction function, Object[] arguments) {
        return new ErlProcess(context, null, null, function, arguments);
    }

    public static ErlProcess spawn(ErlContext context, ErlProcess linkWith, ErlFunction function, Object[] arguments) {
        return new ErlProcess(context, linkWith, null, function, arguments);
    }

    public static ErlProcess spawn(ErlContext context, ErlProcess linkWith, ErlRef[] monitorRef, ErlFunction function, Object[] arguments) {
        return new ErlProcess(context, linkWith, monitorRef, function, arguments);
    }

    public ErlPid getPid() {
        return pid;
    }

    public Future<Object> getFuture() {
        return future;
    }

    public static boolean getTrapExit() {
        return getCurrentProcess().trapExit;
    }

    public static void halt() {
        final ErlProcess proc = getCurrentProcess();
        proc.processManager.close(proc);
    }

    public ErlPid getGroupLeader() {
        return groupLeader;
    }

    public static void setGroupLeader(ErlPid leaderPid, ErlPid pid) {
        final ProcessManager pm = getCurrentProcess().processManager;
        synchronized (pm.processes) {
            ErlProcess proc = pm.findProcess(pid);

            if (null == proc) {
                throw ErlControlException.makeBadarg();
            }

            proc.setGroupLeader(leaderPid);
        }
    }

    private void setGroupLeader(ErlPid leaderPid) {
        synchronized (processManager.processes) {

            if (!leaderPid.equals(pid) && null == processManager.findProcess(leaderPid)) {
                throw ErlControlException.makeBadarg();
            }

            groupLeader = leaderPid;
        }
    }

    public static ErlPid getSelfPid() {
        return ProcessManager.getCurrentProcess().pid;
    }

    public static ErlProcess getCurrentProcess() {
        return ProcessManager.getCurrentProcess();
    }

    public static ErlContext getContext() {
        return ProcessManager.getCurrentProcess().context;
    }

    public static void yield() {
        try {
            ((Callable<Object>) ProcessManager.getCurrentProcess()).wait(1);
        } catch (InterruptedException e) {
            throw ErlExitProcessException.INSTANCE;
        }
    }

    public static ErlProcess findProcess(ErlPid pid) {
        return getCurrentProcess().processManager.findProcess(pid);
    }

    public static Registrable findRegistered(String name) {
        return getCurrentProcess().processManager.findRegisteredName(name);
    }

    public static <T> T findRegistered(Class<T> registrable, String name) {
        return getCurrentProcess().processManager.findRegisteredName(registrable, name);
    }

    public static boolean isProcessAlive(ErlPid pid) {
        return null != getCurrentProcess().processManager.findProcess(pid);
    }

    @Override
    public String getRegisteredName() {
        return registeredName;
    }

    @Override
    public void setRegisteredName(String registeredName) {
        this.registeredName = registeredName;
    }

    @Override
    public void onUnregistered() {
        registeredName = null;
    }

    public boolean register(String name) {
        return processManager.register(name, this);
    }

    public static boolean register(String name, ErlPort port) {
        return getCurrentProcess().processManager.register(name, port);
    }

    public static boolean unregister(String name) {
        final ErlProcess proc = getCurrentProcess();
        if (null != proc) {
            return proc.processManager.unregister(name);
        } else {
            return false;
        }
    }

    public static Set<String> getRegisteredNames() {
        return getCurrentProcess().processManager.getRegisteredNames();
    }

    public static Set<ErlPid> getAllPid() {
        return getCurrentProcess().processManager.getAllPid();
    }

    public boolean isDone() {
        return future.isDone();
    }

    public void addTable(ErlTable tab) {
        tables.add(tab);
    }

    public void removeTable(ErlTable tab) {
        tables.remove(tab);
    }

    public void link(ErlPid connectToPid) {

        if (pid.equals(connectToPid)) {
            return;
        }

        ErlProcess other = processManager.findProcess(connectToPid);

        if (null != other) {
            link(other);
        }
    }

    private void link(ErlProcess other) {
        synchronized (processManager.processes) {

            links.add(other);
            other.links.add(this);
        }
    }

    public void unlink(ErlPid connectToPid) {

        if (pid.equals(connectToPid)) {
            return;
        }

        ErlProcess other = processManager.findProcess(connectToPid);

        if (null != other) {
            unlink(other);
        }
    }

    private void unlink(ErlProcess other) {
        synchronized (processManager.processes) {

            links.remove(other);
            other.links.remove(this);
        }
    }

    private static final class SendAfter implements Callable<Object> {

        final ErlRef ref;
        final long timeout;
        final ErlPid pid;
        final Object msg;

        public SendAfter(long timeout, ErlPid destPid, Object msg) {
            this.ref = ErlRef.make();
            this.timeout = timeout;
            this.pid = destPid;
            this.msg = msg;
        }

        public Object call() throws Exception {

            Thread.sleep(timeout);

            final ErlProcess proc = ErlProcess.findProcess(pid);
            if (null != proc) {
                proc.sendMessage(msg, false);
            }

            return null;
        }
    }

    private static class TimerRef {

        final Future<Object> future;
        private final long expirationTime;

        public TimerRef(Future<Object> future, long timeout) {
            this.future = future;
            this.expirationTime = TimeUnit.NANOSECONDS.toMillis(System.nanoTime()) + timeout;
        }

        public long getTimeLeft() {
            return expirationTime - TimeUnit.NANOSECONDS.toMillis(System.nanoTime());
        }
    }

    public static ErlRef sendAfter(long timeout, ErlPid destPid, Object msg) {

        SendAfter sa = new SendAfter(timeout, destPid, msg);
        final ProcessManager pm = getCurrentProcess().processManager;
        final Future<Object> future = pm.threadPool.submit(sa);

        synchronized (pm.processes) {
            pm.timers.put(sa.ref, new TimerRef(future, timeout));
            return sa.ref;
        }
    }

    public static ErlRef sendAfter(long timeout, ErlAtom destName, Object msg) {

        ErlProcess proc = ErlProcess.findRegistered(ErlProcess.class, destName.getValue());
        ErlPid destPid = ErlPid.USELESS;

        if (null != proc) {
            destPid = proc.pid;
        }

        return sendAfter(timeout, destPid, msg);
    }

    public static Long cancelTimer(ErlRef ref) {
        final ProcessManager pm = getCurrentProcess().processManager;
        synchronized (pm.processes) {
            final TimerRef timer = pm.timers.remove(ref);

            if (null != timer) {
                timer.future.cancel(true);
                return timer.getTimeLeft();
            }

            return null;
        }
    }

    public ErlTuple getInfo(ProcInfoItem item) {

        final Object value;

        switch (item) {
            case REGISTERED_NAME: {
                final String name = getRegisteredName();
                if (null != name) {
                    value = new ErlAtom(name);
                } else {
                    value = ErlList.NIL;
                }
                break;
            }

            case DICTIONARY: {
                ErlContext.PairListBuilderBiConsumer builder = new ErlContext.PairListBuilderBiConsumer();
                dictForEachImpl(builder);
                value = builder.getResult();
                break;
            }

            case MESSAGES: {
                ErlContext.ListBuilderConsumer builder = new ErlContext.ListBuilderConsumer();
                forEachMessagesImpl(builder);
                value = builder.getResult();
                break;
            }

            case LINKS: {
                ErlContext.ListBuilderConsumer builder = new ErlContext.ListBuilderConsumer();
                forEachLinkImpl(builder);
                value = builder.getResult();
                break;
            }

            case STATUS: {
                if (ErlProcess.getCurrentProcess() == this) {
                    value = ErlAtom.RUNNING;
                } else {
                    value = ErlAtom.RUNNABLE;
                }
                break;
            }

            case TRAP_EXIT: {
                value = trapExit;
                break;
            }

            case GROUP_LEADER: {
                value = groupLeader;
                break;
            }

            case HEAP_SIZE: {
                value = 50000L;
                break;
            }

            case STACK_SIZE: {
                value = 42L;
                break;
            }

            case REDUCTIONS: {
                value = 0L;
                break;
            }

            default: {
                ErlContext.notImplemented();
                throw ErlControlException.makeBadarg();
            }
        }

        return new ErlTuple(item.atom, value);
    }

    public Object send(ErlPid destPid, Object msg, boolean nosuspend, boolean noconnect) {

        ErlProcess proc = processManager.findProcess(destPid);

        if (null != proc) {
            if (proc.sendMessage(msg, nosuspend)) {
                return ErlAtom.NOSUSPEND;
            }
        } else if (noconnect) {
            return ErlAtom.NOCONNECT;
        }

        return ErlAtom.OK;
    }

    public Object send(ErlAtom destName, Object msg, boolean nosuspend, boolean noconnect) {

        ErlProcess proc = processManager.findRegisteredName(ErlProcess.class, destName.getValue());

        if (null != proc) {
            if (proc.sendMessage(msg, nosuspend)) {
                return ErlAtom.NOSUSPEND;
            }
        } else if (noconnect) {
            return ErlAtom.NOCONNECT;
        }

        return ErlAtom.OK;
    }

    public void sendMonitorDown(ErlRef ref, ErlPid sender, Object reason) {
        // we must remove the monitor reference from the foreign monitors
        foreignMonitors.remove(ref);

        // send the tuple to the process
        sendMessage(new ErlTuple(ErlAtom._DOWN, ref, ErlAtom.PROCESS, sender, reason), false);
    }

    private boolean sendMessage(Object msg, boolean nosuspend) {
        try {
            // System.err.println("" + pid + " ** got a message: " + msg);

            if (nosuspend && !messageQueueIn.offer(msg)) {
                return true;
            }

            messageQueueIn.putLast(msg);
            return false;

        } catch (InterruptedException e) {
            throw ErlExitProcessException.INSTANCE;
        }
    }

    private Message receiveMessage() {
        try {
            final Object term = messageQueueIn.takeFirst();
            if (null != term) {
                // System.err.println("" + pid + " ** received: " + msg);
                final Message msg = new Message(term);
                messageQueue.addLast(msg);
                return msg;
            }
            return null;
        } catch (InterruptedException e) {
            throw ErlExitProcessException.INSTANCE;
        }
    }

    private Message receiveMessage(long msec) {
        try {
            final Object term = messageQueueIn.pollFirst(msec, TimeUnit.MILLISECONDS);
            if (null != term) {
                // System.err.println("" + pid + " ** received: " + msg);
                final Message msg = new Message(term);
                messageQueue.addLast(msg);
                return msg;
            }
            return null;
        } catch (InterruptedException e) {
            throw ErlExitProcessException.INSTANCE;
        }
    }

    public static interface MessageConsumer {

        /**
         * Inspect the message, and transform it (consumes it).
         *
         * @param msg The received message.
         * @return <code>null</code> if the message is not important; non-null if it is consumed
         */
        public Object accept(Object msg);
    }

    /**
     * Receives a message. Can wait for the desired timeout.
     *
     * @param timeoutMsec timeout value in milliseconds; -1 if infinity
     * @param consumer the consumer that decides the message is needed or not
     * @return the transformed value by the consumer
     */
    public static Object receiveMessage(long timeoutMsec, MessageConsumer consumer) {

        ErlProcess proc = ProcessManager.getCurrentProcess();

        if (!proc.messageQueueIn.isEmpty()) {
            while (null != proc.receiveMessage(0)) {
                // just move the freshly received messages into the permanent queue
            }
        }

        for (Iterator<Message> iter = proc.messageQueue.iterator(); iter.hasNext();) {
            final Message msg = iter.next();
            if (msg.present) {
                msg.present = false;
                try {
                    final Object result = consumer.accept(msg.term);
                    if (null != result) {
                        proc.messageQueue.remove(msg);
                        return result;
                    } else {
                        msg.present = true;
                    }
                } catch (ErlTailCallException ex) {
                    proc.messageQueue.remove(msg);
                    throw ex;
                }
            }
        }

        final long startTime = System.nanoTime();
        final long timeoutNanos = TimeUnit.MILLISECONDS.toNanos(timeoutMsec);

        while (timeoutMsec < 0 || (System.nanoTime() - startTime) <= timeoutNanos) {

            final Message msg;

            if (timeoutMsec < 0) {
                msg = proc.receiveMessage();
            } else {
                msg = proc.receiveMessage(TimeUnit.NANOSECONDS.toMillis(startTime + timeoutNanos - System.nanoTime()));
            }

            if (null == msg) {
                continue;
            }

            msg.present = false;

            try {
                final Object result = consumer.accept(msg.term);
                if (null != result) {
                    proc.messageQueue.remove(msg);
                    return result;
                } else {
                    msg.present = true;
                }
            } catch (ErlTailCallException ex) {
                proc.messageQueue.remove(msg);
                throw ex;
            }
        }

        return null;
    }

    public static void forEachMessages(Consumer<Object> action) {
        ProcessManager.getCurrentProcess().forEachMessagesImpl(action);
    }

    private void forEachMessagesImpl(Consumer<Object> action) {

        if (!messageQueueIn.isEmpty()) {
            while (null != receiveMessage(0)) {
                // just move the freshly received messages into the permanent queue
            }
        }

        for (Iterator<Message> iter = messageQueue.iterator(); iter.hasNext();) {
            final Message msg = iter.next();
            if (msg.present) {
                action.accept(msg.term);
            }
        }
    }

    public static void forEachLink(Consumer<Object> action) {
        ProcessManager.getCurrentProcess().forEachLinkImpl(action);
    }

    private void forEachLinkImpl(Consumer<Object> action) {

        for (Iterator<ErlProcess> iter = links.iterator(); iter.hasNext();) {
            action.accept(iter.next().getPid());
        }
    }

    public static Object dictPut(Object key, Object value) {
        ErlProcess proc = ProcessManager.getCurrentProcess();
        synchronized (proc.dictionary) {
            return proc.dictionary.put(key, value);
        }
    }

    public static Object dictGet(Object key) {
        ErlProcess proc = ProcessManager.getCurrentProcess();
        synchronized (proc.dictionary) {
            return proc.dictionary.get(key);
        }
    }

    public static Object dictErase(Object key) {
        ErlProcess proc = ProcessManager.getCurrentProcess();
        synchronized (proc.dictionary) {
            return proc.dictionary.remove(key);
        }
    }

    public static void dictEraseAll(BiConsumer<Object, Object> action) {
        ErlProcess proc = ProcessManager.getCurrentProcess();
        synchronized (proc.dictionary) {
            proc.dictionary.forEach(action);
            proc.dictionary.clear();
        }
    }

    public static void dictForEach(BiConsumer<Object, Object> action) {
        ProcessManager.getCurrentProcess().dictForEachImpl(action);
    }

    private void dictForEachImpl(BiConsumer<Object, Object> action) {
        synchronized (dictionary) {
            dictionary.forEach(action);
        }
    }

    public Object processFlag(ErlAtom flag, Object value) {

        if (getCurrentProcess() == this) {

            if (0 == ErlAtom.compare(ErlAtom.TRAP_EXIT, flag)) {
                boolean old = trapExit;
                trapExit = ErlContext.decodeBoolean(ErlAtom.fromObject(value));
                return old;
            }

            if (0 == ErlAtom.compare(ErlAtom.MONITOR_NODES, flag)) {
                boolean old = monitorNodes;
                monitorNodes = ErlContext.decodeBoolean(ErlAtom.fromObject(value));
                return old;
            }

            if (0 == ErlAtom.compare(ErlAtom.PRIORITY, flag)) {
                if (ErlAtom.NORMAL.equals(value) || ErlAtom.LOW.equals(value) || ErlAtom.HIGH.equals(value) || ErlAtom.MAX.equals(value)) {
                    ErlAtom old = priority;
                    priority = (ErlAtom) value;
                    return old;
                }

                throw ErlControlException.makeBadarg();
            }

            ErlContext.notImplemented();
        }

        throw ErlControlException.makeBadarg();
    }

    public static ErlRef monitor(ErlPid item) {

        ErlProcess curr = getCurrentProcess();
        ErlProcess proc = ErlProcess.findProcess(item);
        ErlRef ref = ErlRef.make();

        synchronized (curr.processManager.processes) {

            if (null != proc) {
                proc.monitors.put(ref, curr);
                curr.foreignMonitors.put(ref, proc);
            } else {
                getCurrentProcess().sendMonitorDown(ref, item, ErlAtom.NOPROC);
            }
        }

        return ref;
    }

    public static boolean demonitor(ErlRef ref, boolean flush, boolean info) {

        ErlProcess curr = getCurrentProcess();

        boolean removed;

        synchronized (curr.processManager.processes) {

            ErlProcess item = curr.foreignMonitors.get(ref);

            if (null != item) {
                item.monitors.remove(ref);
                curr.foreignMonitors.remove(ref);
                removed = true;
            } else {
                removed = false;
            }
        }

        boolean flushed = false;

        if (flush) {

            if (!curr.messageQueueIn.isEmpty()) {
                while (null != curr.receiveMessage(0)) {
                    // just move the freshly received messages into the permanent queue
                }
            }

            for (Iterator<Message> iter = curr.messageQueue.iterator(); iter.hasNext();) {

                final Message msg = iter.next();

                if (msg.present && msg.term instanceof ErlTuple) {

                    ErlTuple tuple = (ErlTuple) msg.term;

                    if (5 == tuple.getSize() && (tuple.getElement(2) instanceof ErlRef) && 0 == ref.compare((ErlRef) tuple.getElement(2))) {
                        curr.messageQueue.remove(msg);
                        flushed = true;
                        break;
                    }
                }
            }

            if (info) {
                return !flushed;
            }
        } else if (info) {
            return removed;
        }

        return true;
    }

    public static void kill(ErlPid pid, Object reason) {

        final ErlProcess proc = findProcess(pid);

        if (null != proc) {
            proc.killProcess(getSelfPid(), new ErlTuple(ErlAtom._EXIT, reason));
        }
    }

    protected void killProcess(ErlPid sender, Object reason) {

        exitReason = reason;

        if (sender == pid) {

            if (trapExit) {
                ErlTuple msg = ((ErlTuple) exitReason).makeInserted(2, sender);
                sendMessage(msg, false);
            } else if (!ErlTuple.EXIT_NORMAL.equals(exitReason)) {
                future.cancel(true);
            }

        } else {

            if (ErlTuple.EXIT_NORMAL.equals(exitReason)) {
                if (trapExit) {
                    ErlTuple msg = ((ErlTuple) exitReason).makeInserted(2, sender);
                    sendMessage(msg, false);
                }
            } else if (ErlTuple.EXIT_KILL.equals(exitReason)) {
                exitReason = ErlTuple.EXIT_KILLED;
                future.cancel(true);
            } else if (trapExit) {
                ErlTuple msg = ((ErlTuple) exitReason).makeInserted(2, sender);
                sendMessage(msg, false);
            } else {
                future.cancel(true);
            }

        }
    }

    @Override
    public int hashCode() {
        return pid.hashCode();
    }

    public static void addPort(ErlPort port) {

        final ProcessManager pm = getCurrentProcess().processManager;

        synchronized (pm.processes) {
            pm.ports.add(port);
        }
    }

    public static void removePort(ErlPort port) {

        final ErlProcess proc = getCurrentProcess();

        if (null != proc) {

            final ProcessManager pm = proc.processManager;

            synchronized (pm.processes) {
                pm.ports.remove(port);
            }
        }
    }

    public static ErlList buildBackTraceAsList() {
        ErlList result = ErlList.NIL;

        Iterator<MFA> iter = getCurrentProcess().callStack.descendingIterator();
        while (iter.hasNext()) {
            result = new ErlList(iter.next().toTuple(), result);
        }

        return result;
    }

    private static final int LOG_LEVEL;

    static {
        LOG_LEVEL = 0;
    }

    public static void funcEnter(MFA mfa, VirtualFrame frame) {
        // if (LOG_LEVEL >= 2) {
        // System.err.println("" + getCurrentProcess().pid + " enter " + mfa);
        // }
        getCurrentProcess().callStack.push(mfa);

        // if (LOG_LEVEL >= 3) {
        // Object[] args = frame.getArguments();
        // for (int i = 0, n = mfa.getArity(); i < n; ++i) {
        // ErlContext.getTermRank(args[i]);
        // System.err.println("" + getCurrentProcess().pid + " arg[" + i + "] = " + args[i]);
        // }
        // }
    }

    public static void funcLeave() {
        // if (LOG_LEVEL >= 2) {
        // System.err.println("" + getCurrentProcess().pid + " leave " +
        // getCurrentProcess().callStack.getFirst());
        // }
        getCurrentProcess().callStack.pop();
    }

    @Override
    public String toString() {
        return "" + pid + "/" + registeredName + " (initial=" + initialFunction + ", reason=" + exitReason + ")";
    }

    public static Object evaluate(ErlFunction function, Object... arguments) {
        return getCurrentProcess().evaluateInProcess(function, arguments);
    }

    @Override
    public Object call() {
        if (LOG_LEVEL >= 1) {
            System.err.println("" + pid + " started at " + initialFunction);
        }
        ProcessManager.processEntry(this);
        try {
            return evaluateInProcess(initialFunction, initialArguments);
        } catch (ErlControlException ex) {
            exitReason = ex.getDescribingTerm();
            throw ex;
        } catch (ErlExitProcessException ex) {
            return null;
        } catch (Exception ex) {
            ex.printStackTrace();
            throw ex;
        } finally {
            if (LOG_LEVEL >= 1) {
                System.err.println("" + pid + " exited with " + exitReason);
            }
            processManager.processExit(this, exitReason);
        }
    }

    private Object evaluateInProcess(ErlFunction function, Object... arguments) {
        try {

            ErlFunction func = function;
            Object[] args = arguments;

            for (;;) {

                if (null != func && func.isCallable()) {

                    try {
                        args = Arrays.copyOf(args, args.length + 1);
                        args[args.length - 1] = func.getContext();

                        return func.getCallTarget().call(args);
                    } catch (ErlTailCallException tailCallEx) {

                        func = tailCallEx.getFunction();
                        args = tailCallEx.getArguments();
                    }

                } else {
                    ErlList desc = new ErlList(new ErlTuple(func.getModule(), func.getName(), func.getArity()), ErlList.NIL);
                    ErlControlException ex = ErlControlException.makeUndef(desc);
                    exitReason = ex.getDescribingTerm();
                    throw ex;
                }
            }

        } catch (ErlControlException ex) {
            exitReason = ex.getDescribingTerm();
            throw ex;
        }
    }
}

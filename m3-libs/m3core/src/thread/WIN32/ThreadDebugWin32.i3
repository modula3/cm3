INTERFACE ThreadDebugWin32;

FROM Thread IMPORT Mutex, Condition, T;

<*EXTERNAL ThreadDebugWin32__LockMutex*>
PROCEDURE LockMutex (m: Mutex);

<*EXTERNAL ThreadDebugWin32__UnlockMutex*>
PROCEDURE UnlockMutex(m: Mutex);

<*EXTERNAL ThreadDebugWin32__InnerWait*>
PROCEDURE InnerWait(m: Mutex; c: Condition; self: T);

<*EXTERNAL ThreadDebugWin32__InnerTestAlert*>
PROCEDURE InnerTestAlert(self: T);

<*EXTERNAL ThreadDebugWin32__AlertWait*>
PROCEDURE AlertWait (m: Mutex; c: Condition);

<*EXTERNAL ThreadDebugWin32__Wait*>
PROCEDURE Wait (m: Mutex; c: Condition);

<*EXTERNAL ThreadDebugWin32__DequeueHead*>
PROCEDURE DequeueHead(c: Condition);

<*EXTERNAL ThreadDebugWin32__Signal*>
PROCEDURE Signal (c: Condition);

<*EXTERNAL ThreadDebugWin32__Broadcast*>
PROCEDURE Broadcast (c: Condition);

<*EXTERNAL ThreadDebugWin32__Alert*>
PROCEDURE Alert(t: T);

<*EXTERNAL ThreadDebugWin32__XTestAlert*>
PROCEDURE XTestAlert (self: T);

<*EXTERNAL ThreadDebugWin32__TestAlert*>
PROCEDURE TestAlert();

<*EXTERNAL ThreadDebugWin32__Join*>
PROCEDURE Join(t: T);

<*EXTERNAL ThreadDebugWin32__AlertJoin*>
PROCEDURE AlertJoin(t: T);

<*EXTERNAL ThreadDebugWin32__Fork*>
PROCEDURE Fork();

<*EXTERNAL ThreadDebugWin32__RunThread*>
PROCEDURE RunThread();

END ThreadDebugWin32.

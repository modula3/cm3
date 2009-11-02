INTERFACE ThreadDebug;

FROM Thread IMPORT Mutex, Condition, T;

<*EXTERNAL ThreadDebug__LockMutex*>
PROCEDURE LockMutex (m: Mutex);

<*EXTERNAL ThreadDebug__UnlockMutex*>
PROCEDURE UnlockMutex(m: Mutex);

<*EXTERNAL ThreadDebug__InnerWait*>
PROCEDURE InnerWait(m: Mutex; c: Condition; self: T);

<*EXTERNAL ThreadDebug__InnerTestAlert*>
PROCEDURE InnerTestAlert(self: T);

<*EXTERNAL ThreadDebug__AlertWait*>
PROCEDURE AlertWait (m: Mutex; c: Condition);

<*EXTERNAL ThreadDebug__Wait*>
PROCEDURE Wait (m: Mutex; c: Condition);

<*EXTERNAL ThreadDebug__DequeueHead*>
PROCEDURE DequeueHead(c: Condition);

<*EXTERNAL ThreadDebug__Signal*>
PROCEDURE Signal (c: Condition);

<*EXTERNAL ThreadDebug__Broadcast*>
PROCEDURE Broadcast (c: Condition);

<*EXTERNAL ThreadDebug__Alert*>
PROCEDURE Alert(t: T);

<*EXTERNAL ThreadDebug__XTestAlert*>
PROCEDURE XTestAlert (self: T);

<*EXTERNAL ThreadDebug__TestAlert*>
PROCEDURE TestAlert();

<*EXTERNAL ThreadDebug__Join*>
PROCEDURE Join(t: T);

<*EXTERNAL ThreadDebug__AlertJoin*>
PROCEDURE AlertJoin(t: T);

<*EXTERNAL ThreadDebug__Fork*>
PROCEDURE Fork();

<*EXTERNAL ThreadDebug__RunThread*>
PROCEDURE RunThread();

<*EXTERNAL ThreadDebug__LockHeap*>
PROCEDURE LockHeap();

<*EXTERNAL ThreadDebug__UnlockHeap*>
PROCEDURE UnlockHeap();

<*EXTERNAL ThreadDebug__WaitHeap*>
PROCEDURE WaitHeap();

<*EXTERNAL ThreadDebug__BroadcastHeap*>
PROCEDURE BroadcastHeap();

END ThreadDebug.

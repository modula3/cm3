INTERFACE ThreadDebug;

<*EXTERNAL ThreadDebug__LockMutex*>
PROCEDURE LockMutex (mutex: REFANY);

<*EXTERNAL ThreadDebug__UnlockMutex*>
PROCEDURE UnlockMutex(mutex: REFANY);

<*EXTERNAL ThreadDebug__InnerWait*>
PROCEDURE InnerWait(mutex: REFANY; condition: REFANY; self: REFANY);

<*EXTERNAL ThreadDebug__InnerTestAlert*>
PROCEDURE InnerTestAlert(self: REFANY);

<*EXTERNAL ThreadDebug__AlertWait*>
PROCEDURE AlertWait (mutex: REFANY; condition: REFANY);

<*EXTERNAL ThreadDebug__Wait*>
PROCEDURE Wait (mutex: REFANY; condition: REFANY);

<*EXTERNAL ThreadDebug__DequeueHead*>
PROCEDURE DequeueHead(condition: REFANY);

<*EXTERNAL ThreadDebug__Signal*>
PROCEDURE Signal (condition: REFANY);

<*EXTERNAL ThreadDebug__Broadcast*>
PROCEDURE Broadcast (condition: REFANY);

<*EXTERNAL ThreadDebug__Alert*>
PROCEDURE Alert(thread: REFANY);

<*EXTERNAL ThreadDebug__XTestAlert*>
PROCEDURE XTestAlert (self: REFANY);

<*EXTERNAL ThreadDebug__TestAlert*>
PROCEDURE TestAlert();

<*EXTERNAL ThreadDebug__Join*>
PROCEDURE Join(thread: REFANY);

<*EXTERNAL ThreadDebug__AlertJoin*>
PROCEDURE AlertJoin(thread: REFANY);

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

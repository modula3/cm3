UNSAFE MODULE UnsafeMutex;
IMPORT Thread;
IMPORT RTCollector;

TYPE
  IKnowWhatMutexIs = OBJECT
    holder, waitingForMe: Thread.T;
  END;

PROCEDURE CheckMyLock(m: MUTEX): BOOLEAN =
  VAR
    mutex: IKnowWhatMutexIs;
    result: BOOLEAN;
  BEGIN
    RTCollector.Disable();
    mutex := LOOPHOLE(m, IKnowWhatMutexIs);
    result := mutex.holder = Thread.Self();
    RTCollector.Enable();
    RETURN result;
  END CheckMyLock; 

PROCEDURE AssertMyLock(m: MUTEX) =
  BEGIN
    <* ASSERT CheckMyLock(m) *>
  END AssertMyLock;

PROCEDURE GetHolder(m: MUTEX): Thread.T =
  BEGIN
    RETURN LOOPHOLE(m, IKnowWhatMutexIs).holder;
  END GetHolder;

BEGIN
END UnsafeMutex.

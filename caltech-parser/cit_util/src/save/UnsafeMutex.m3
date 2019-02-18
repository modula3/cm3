UNSAFE MODULE UnsafeMutex;
IMPORT Thread;
IMPORT RTCollextor;

TYPE
  IKnowWhatMutexIs = OBJECT
    holder, waitingForMe: Thread.T;
  END;

PROCEDURE CheckMyLock(m: MUTEX): BOOLEAN =
  VAR
    mutex: IKnowWhatMutexIs;
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

BEGIN
END UnsafeMutex.

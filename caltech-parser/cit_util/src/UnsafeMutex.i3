INTERFACE UnsafeMutex;
IMPORT Thread;
PROCEDURE AssertMyLock(m: MUTEX);
PROCEDURE GetHolder(m: MUTEX): Thread.T;
END UnsafeMutex.

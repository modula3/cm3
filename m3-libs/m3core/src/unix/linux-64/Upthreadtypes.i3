INTERFACE Upthreadtypes;

TYPE
  pthread_attr_t = ARRAY[1..7] OF INTEGER;
  pthread_mutex_t = ARRAY[1..5] OF INTEGER;
  pthread_rwlock_t = ARRAY[1..9] OF INTEGER;
  pthread_rwlockattr_t = INTEGER;

END Upthreadtypes.

INTERFACE Upthreadtypes;

TYPE
  pthread_attr_t = ARRAY[1..9] OF INTEGER;
  pthread_mutex_t = ARRAY[1..6] OF INTEGER;
  pthread_rwlock_t = ARRAY[1..8] OF INTEGER;
  pthread_rwlockattr_t = ARRAY[1..2] OF INTEGER;

END Upthreadtypes.

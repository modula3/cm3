(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * See file COPYRIGHT-COLUMBIA for details.
 * 
 * Author          : Blair MacIntyre
 * Created On      : Mon May 15 17:26:50 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Wed Nov 27 19:23:10 1996
 * Update Count    : 23
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/RdWrMutex.i3,v $
 * $Date: 2001-12-02 00:20:38 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.4  1997/01/23 15:26:41  bm
 * Lots of little bug fixes.
 *
 * Revision 1.3  1996/11/21 22:51:14  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* A "RdWrMutex.T" is a mutex that supports being locked by a single
   "writer" or by multiple "readers".  The mutex has the following
   properties: 
   \begin{itemize}
   \item The locks are associated with the thread that calls
   "acquireRead" or "acquireWrite".  Attempting to release the mutex
   with a different thread is a runtime error.
   \item It is safe to reacquire a held lock, as long as the lock is
   released the same number of times, with reads and writes released
   in the same order.  This includes any combination or acquiring read
   and write locks.
   \item If the owner of a read lock attempts to reacquire a write
   lock on the mutex, it will block until all other read locks have
   been released.
   \end{itemize}

   The "wait" method can be called when a read lock, a write lock or
   neither has been acquired.  All locks will be released while
   sleeping on the condition variable "cv."  It is assumed the
   argument "mu" is held before "wait" is called.

   Here's an idea to make them more fair.  The only potential
   unfairness is that if a reader holds a "RdWrMutex.T," a writer
   could block indefinately as long as at least one reader keeps the
   lock.   If, however, we order the queue waiting for the lock in a
   first-come-first-server manner, and only let multiple readers in if
   one of the readers at the front of the queue still has the lock, we
   will be more fair.  When we say "at the front of the queue," we
   mean those readers that aquired the read lock before the first
   writer that is waiting for it.
*)

INTERFACE RdWrMutex;

IMPORT AtomList, Thread;

EXCEPTION Error(AtomList.T);

TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(): T;
    acquireRead();
    acquireWrite();
    releaseRead();
    releaseWrite();

    wait(mu: Thread.Mutex; cv: Thread.Condition);
  END;

END RdWrMutex.

(*                            -*- Mode: Modula-3 -*- 
 * 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 * 
 * Author          : Blair MacIntyre
 * Created On      : Wed May 10 15:36:50 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Oct 23 11:41:07 1997
 * Update Count    : 30
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/WorkerPool.i3,v $
 * $Date: 2001-12-02 00:06:45 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.4  1997/10/24 19:31:35  bm
 * Added the ability to flush the readers and worker pool.
 *
 * Revision 1.3  1997/02/17 16:22:43  bm
 * fixed comment
 *
 * Revision 1.2  1997/01/23 15:26:43  bm
 * Lots of little bug fixes.
 *
 * 
 * HISTORY
 *)

(* A simple interface to create a pool of workers. A worker is a
   thread which handles pieces of "work" which are supplied as
   subclasses of "Work.T" objects.  The subtypes provide a "handle()"
   method for each unit of work.  Since a unit of "Work.T" is an
   object, it can contain any other data or methods needed to
   accomplish its task.

   After a pool of workers has been initialized, simply add a unit of
   work to be performed by calling "add(work)".  An idle thread will
   handle it, or a new thread will be created if none are idle, and
   there are less than maxThreads already performing work.  There will
   be at most maxIdleThreads threads idle at any given time.  By
   default, the maximum number of idle threads is a function of the
   maximum number of threads.  Increasing this will increase both
   memory overhead (by having idle threads tie up memory) and (in the
   current implementation) CPU overhead because all idle threads are
   awakened when new work arrives.  "stackSize" is the stackSize of
   the worker threads.  The default is the system default (usually
   3000).  If your threads run out of space, you may want to increase
   this.

   If a worker thread needs to remove itself from the pool of workers,
   either permanently or temporarily (perhaps because it will be
   blocking for an unexpectedly large period of time), it should call
   "stealWorker()."  The thread will be removed from the list of
   workers, allowing another thread to be added to the pool.  When a
   stolen thread finishes its work, it will either be destroyed or
   added to the idle queue, depending on the number of threads left
   around.  If is ok, but meaningless, for a non-worker thread to call
   "stealWorker()."  The return value is true is the thread was a
   worker thread, false otherwise.

   Call flush() when you want to pause until all the work has been
   done.   You can wake a blocked thread by alerting it.

   Call finish() when you wish to wait for all the worker threads to
   finish what they are doing.
*)

INTERFACE WorkerPool;

IMPORT Work, Thread;

TYPE
  T <: Public;
  Public = Private OBJECT METHODS
    init(maxThreads: CARDINAL := 4;
         maxIdleThreads: INTEGER := -1; 
         stackSize: CARDINAL := 0): T;
    add(work: Work.T);
    stealWorker(): BOOLEAN;
    flush() RAISES {Thread.Alerted};
    finish();
  END;
  Private <: ROOT;

END WorkerPool.

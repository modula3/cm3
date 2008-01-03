(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "NotifyQueue" interface is for the board server to log and
   retrieve notify records. 
*)

INTERFACE NotifyQueue;

IMPORT NotifyRec;

TYPE T <: Public;
 
     Public = Private OBJECT METHODS
       init (): T;
       enq (nr: NotifyRec.T);
       deq (): NotifyRec.T;
     END;   

     Private <: ROOT;

(* The method "enq" inserts the given notify record at the tail of the
   queue. However, the implementation may {\em merge} records for
   efficiency such that the effect of the notifications is not
   altered. For example, if there already exists a "Create" or
   "Modify" record, there is no need to log another "Modify" record by
   the same "doer" for the same item. 

   The method "deq" blocks until a record is available in the queue,
   and then deletes one from the head of the queue and returns it.

   Calls to "enq" and "deq" are atomic in that they can be called
   concurrently. (In the implementation, records are enqueued in
   response to client calls; a separate thread running in the
   background dequeues the records.)
*)

END NotifyQueue.

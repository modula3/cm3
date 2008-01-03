(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*| Last modified on Tue Jan  3 08:15:30 PST 1995 by kalsow  *)
(*|      modified on Thu Nov 18 16:17:31 PST 1993 by mcjones *)
(*|      modified on Thu Apr 29 16:56:59 1993 by gnelson     *)
(*|      modified on Fri Feb 12 10:44:32 PST 1993 by jdd     *)

(* Most Modula-3 programs simply let the garbage collector deallocate
   storage automatically, but some programs need more control.  
   For example, if a variable allocated in the traced
   heap contains a handle on a resource in the operating system or in
   some other address space, then when the variable is garbage-collected
   it may be important to deallocate the resource.  The "WeakRef"
   interface provides this additional control.
   \index{weak reference}
   \index{deallocating resources}
   \index{garbage collection!weak references}

   A {\it node} is a datum allocated on the traced heap. Thus a node
   is either the referent of a variable of a fixed reference type or
   the data record of a traced object.  Note that a node is not a
   Modula-3 reference, but the allocated storage to which a reference
   can refer.

   A "WeakRef.T" is a data structure that refers to a node 
   without protecting the node from the garbage collector.  If "w" is a 
   weak reference, we write "nd(w)" to denote the node to which "w"
   refers.

   We say that a weak reference "w" {\it dies} at the moment that the
   garbage collector detects that "nd(w)" is unreachable.  A precise
   definition of unreachable is given below.  Once a weak reference
   has died, it remains dead forever, even if the node to which
   it refers becomes reachable again.

   Associated with each weak reference "w" is a {\it cleanup procedure}
   "cp(w)".  If the cleanup procedure is not "NIL", the garbage
   collector will schedule a call to it when the weak reference
   dies. *)

INTERFACE WeakRef;

TYPE T = 
  RECORD
    byte: ARRAY [0..7] OF BITS 8 FOR [0..255]
  END;
(* Please treat this as though it were an opaque type:  the
   only operations allowed are assignment, equality tests,
   and the procedures in this interface.  *)

PROCEDURE FromRef(r: REFANY; p: CleanUpProc := NIL): T;
(* Return a weak reference "w" such that  "nd(w) = r" and 
   "cp(w) = p".  It is a checked runtime error if "r" is "NIL".
   It is illegal to create more than one weak reference with a
   non-nil cleanup to the same node; violations of this rule
   may lead to a checked runtime error, or may cause one
   of the cleanup actions to be omitted.  "FromRef" is not
   necessarily functional: it is possible that "nd(w1) = nd(w2)"
   but "w1 # w2". *)

PROCEDURE ToRef(w: T): REFANY;
(* Return a reference to "nd(w)", unless "w" is dead, in which 
   case return "NIL".  *)

TYPE CleanUpProc = PROCEDURE(READONLY w: T; r: REFANY);
(* If "cp(w)" is not "NIL", then when "w" dies, the garbage collector
   will schedule the call "cp(w)(w, <reference to nd(w)>)".  *)

END WeakRef.

(* The cleanup procedure will be executed at some point after 
   the weak reference dies.   A cleanup procedure is called with 
   no locks held; it must return promptly to allow other 
   objects to be cleaned up.  

   The computation "cp(w)(w, ref)" is allowed to store "ref" in a
   non-local variable, thus making "nd(w)" reachable again; the heap
   storage will not have been freed.  This does not change the fact
   that "w" is dead.  The cleanup procedure can re-enable cleanup, if
   desired, by creating a new weak reference to "nd(w)".

   The storage for a node is reclaimed when it is unreachable
   and all weak references to it are dead and all cleanup calls 
   scheduled for it have been completed.

   Finally we come to the precise definition of ``reachable'':

   A node is {\it reachable} if it can be reached by a path of traced
   references starting from a current procedure activation record, a global
   variable, or a weakly referenced node with a non-nil
   cleanup {\it other than itself}.

   Thus a weak reference to a node "nd" does not make "nd" reachable,
   but if it has a non-nil cleanup, it makes other nodes referenced 
   from "nd" reachable.

   For example, if "A" and "B" are two nodes that are weakly referenced 
   by weak references with non-nil cleanup procedures, then if "B"
   is reachable from "A", then "B" is reachable.  But if "A" is not
   reachable, then the garbage collector will eventually detect this 
   and schedule the cleanup of "A".  If the cleanup call returns 
   without resurrecting "A", then "A"'s storage will be reclaimed, 
   at which point "B" will be unreachable, which will lead to its cleanup.

   If "A" and "B" are weakly referenced nodes with non-nil cleanups
   that are connected by a cycle of traced references, then both of 
   them are reachable.   As long as the cycle persists, neither will 
   be cleaned up.  This situation represents a storage leak and 
   should be avoided.

\subsection*{Examples}

{\bf 1}.\ \  Suppose you want writers of the class "WrX.T" to be automatically
flushed and closed if they become unreachable.  Then you could write
code like the following in the "WrX" module:

| MODULE WrX; IMPORT WeakRef, Wr, ...;
| 
| PROCEDURE New(...): T =
|   VAR res := NEW(T); BEGIN
|     (* ... initialize res as a WrX.T ... *)
|     EVAL WeakRef.FromRef(res, Cleanup);
|     RETURN res
|   END New;
|
| PROCEDURE Cleanup(READONLY self: WeakRef.T; ref: REFANY) =
|   VAR wr: T := ref; BEGIN
|     IF NOT Wr.Closed(wr) THEN
|       Wr.Flush(wr);
|       Wr.Close(wr)
|     END
|   END Cleanup;
| 

There is no danger that another thread could close the writer
after the test "NOT Wr.Closed(wr)" and before the call "Wr.Flush(wr)",
since when "Cleanup" is called, the writer is unreachable.  Therefore 
the cleanup method has exclusive access to the writer.

\smallskip

{\bf 2.}\ \  The network object runtime must map wire
representations for network objects into surrogate objects.  To hand
out the same surrogate for the same wire representation, it keeps a
table mapping wire representations to surrogates.  This table contains
weak references, so the table entry itself does not prevent the
surrogate from being collected.  When the surrogate is collected, it
is removed from the table and the server containing that object is
notified that the client no longer has a surrogate for it.

When a weak reference in the table becomes dead, the network object
represented by the dead surrogate might be unmarshaled by the address
space before the surrogate is cleaned up.  In this case the unmarshaling
code resurrects the unreachable surrogate by creating a new weak 
reference and inserting it in the table in place of the dead weak
reference.  The cleanup code can tell whether to report in clean by
checking whether there is a new weak reference in the table or not.

Here is a sketch of the code:

| TYPE Surrogate = OBJECT wr: WireRep; ... END;

| VAR
|   mu := NEW(MUTEX);
|   <* LL >= {mu} *>
|   tbl := NEW(WireRepToWeakRefTbl.T);

   The mutex "mu" must be held to read or write "tbl" (that
   is what the "LL" pragma means).

   The table "tbl" maps "WireRep"s to "WeakRef"s that reference
   surrogates.

   The following invariants hold whenever "mu" is not held:

   If "tbl(wrep)" is not dead, then "nd(tbl(wrep))" is the 
   surrogate for the network object whose wire representation 
   is "wrep".

   If "tbl(wrep)" is dead, then the surrogate for "wrep" is 
   unreachable. 
   
   If "tbl" has no entry for "wrep", then the address space
   contains no surrogate for "wrep".

| PROCEDURE Cleanup(READONLY wref: WeakRef.T; ref: REFANY) =
| <* LL = {} *>
|   VAR 
|     srg := NARROW(ref, Surrogate); 
|     tblVal: WeakRef.T;
|   BEGIN
|     LOCK mu DO
|       IF tbl.get(srg.wr, tblVal) AND wref = tblVal 
|       THEN
|         EVAL tbl.delete(srg.wr);
|         ... Report that srg is deleted ...
|       END
|     END
|   END Cleanup;
|
| PROCEDURE WireRepToSrg(wrep: WireRep): Surrogate =
|   VAR wref: WeakRef.T; res: Surrogate; BEGIN
|     LOCK mu DO
|       IF tbl.get(wrep, wref) THEN
|         res := WeakRef.ToRef(wref);
|         IF res # NIL THEN RETURN res END
|       END;
|       res := NewSurrogate(wrep);
|       EVAL tbl.put(wrep, WeakRef.FromRef(res, Cleanup));
|       RETURN res
|     END
|   END WireRepToSrg;

  In the above we assume that "NewSurrogate" creates a new surrogate
  from a wire representation. *)


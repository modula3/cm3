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
 * Created On      : Thu Mar 30 17:21:53 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Jan 29 13:00:41 1998
 * Update Count    : 108
 * 
 * $Source$
 * $Date$
 * $Author$
 * $Revision$
 * 
 * $Log$
 * Revision 1.4  1998/05/11 02:34:15  bm
 * bug fixes, added SharedObj.Wait
 *
 * Revision 1.3  1997/03/12 21:50:40  bm
 * Bug fix.
 *
 * Revision 1.2  1996/11/22 19:01:32  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* The shared object package is designed to complement Network
   objects.  A network object allows an object to be shared by
   multiple processes, possibly on different machines, by creating
   surrogate objects in all processes except the one in which the
   object actually exists.  From the point of view of the programmer
   and the executing program, the surrogate object behaves exactly
   like the original object.  However, all method calls to the
   surrogates are synchronously sent to the original object where they
   are executed, with return values or exceptions propogated back to
   the calling process.  

   For many applications, this is sufficient and has many desirable
   properties: there are no sychronization issues, calls are
   synchronous so exceptions propogate across processes, global
   garbage collection is performed, etc.  However, for some
   applicates, the paradigm is not appropriate.  The sychronous nature
   method calls restricts the frequency and of checking the object for
   changes.  More seriously, interested parties are restricted to
   polling an object if they are interested in changes.   Finally, all
   operations on an object, no matter how trivial, require a remote
   procedure call, which takes a significant amount of time.

   To address these problems, and others, the shared object package
   was created.  The model is the opposite of the network object
   package and is intended to complement rather that replace it.
   Instead of the object being stored at one location and remote
   method calls being used to access the object, shared objects are
   fully replicated in all interested processes, with any updates to
   the object being applied to all copies.  

   \section{Differences between Shared and Network Objects} 

   The easiest way to explain the shared object package is to compare
   it to the network object package.  In order to support full
   replication, there are some important behavioural differences
   between network and shared objects.  In the discussion below,
   "update methods" refer to methods which change the internal state
   of an object.  The "local copy" is the copy that resides in the
   process in which the method call is made.

   \begin{list}
   \item Methods which update the shared object are applied to all
   copies of the shared object.  It is important to realize that the
   actual method is called at all sites.  The shared object package
   guarantees that all updates will be applied to all copies of an
   object in the same order, but makes to guarantees about the
   specific order of application of two method calls performed at
   approximately the same time.  
   \item In order to provide the above guarantee, update method calls
   are not performed immediately on the local copy of an object.  The
   caller will block until it is time for the update to be performed,
   then will be unblocked, perform the update, and return.
   \item Restrictions are placed on the kinds of parameters that can
   be used with update methods, due to the requirement that these
   parameters be distributed to all copies so the update method can be
   applied everywhere.  Specifically, network objects, shared objects,
   data streams (subtypes of "Rd.T" or "Wr.T") and any object that is
   associated with a particular process should not be used.  (The
   restriction on using network objects and shared objects as
   arguments to update methods may be lifted someday if absolutely
   necessary).
   \item Network object method calls are performed synchronously.
   Thus, values can be returned and exceptions propogated back to the
   original site.  Shared object update method calls are performed
   synchronously on the local copy, but asynchronously on all other
   copies.  Thus, return values and exceptions (except the
   "SharedObj.Fatal" exception, as described below) are ignored in
   all copies except the local one.  
   \item Non-update method calls are performed immediately on the
   local copy.  Thus, read access to shared objects is significantly
   faster that to network objects.
   \item Since network objects exist at only one location, programmers
   are free to create subtypes of network objects which still appear
   as the original network object when sent out over the network.
   This works because the network object stub generator only generates
   the surrogate objects that stand in for the object on remote
   machines: the real object is not changes.  Shared objects, on the
   other hand, must not be subtyped, as the shared object stub
   generator generates an object which encapsulates the original
   object, overriding all methods to create the desired behaviour.  If
   subtypes are created and distributed, incorrect behaviour may result.
   \item Network objects provide no way for anyone with a copy of the
   network object to be notified when it changes.  It is
   straightforward to add this to a specific network objects
   explicitely in exactly the same way as one would do it to any other
   objects.  All shared objects, on the other hand, have change
   notification built in via "Callback" objects.  When a shared object
   is generated with the stub generator, a corresponding callback
   object is generated, as described below.
   \end{list}.

   \section{The Shared Object Package}
   
   The primary public interface for using shared objects is described here.
*)

INTERFACE SharedObj;

IMPORT Atom, AtomList, Thread, EventNumber, EmbProxiedObj, Wr, 
       Pickle2 AS Pickle, Rd;

CONST Brand = "Shared Objects v.1";

TYPE
  T <: Public;
  Public = EmbProxiedObj.T OBJECT END;

  PROCEDURE Init(s: T): T;

(* "SharedObj.T" is the root type of all shared objects.  All shared
   object types are required to have their init method call the shared
   object "Init" procedure to properly initialize a shared object.
   Furthermore, all shared object are "EmbProxiedObj.T"s, allowing them
   to be embedded in an interpretted language such as Obliq. *)

TYPE Code = AtomList.T;

EXCEPTION Error(Code);           (* Error condition. *)
EXCEPTION Fatal(Code);           (* Fatal error condition. *)

(* Shared objects raise two kinds of exceptions, "Error(code)" and
   "Fatal(code)", where code is an "AtomList.T" describing the
   exception.  "Error" exceptions are algorithmic/logic errors that
   will occur in all copies of an object when a update method is
   applied.  Since it is raised in all copies, they will remain
   sychronized, and the original caller will receive the exception.
   "Fatal()" exceptions, on the other hand, are meant for situations
   that may not occur on all copies of the object, such as out of
   memory or disk space, unavailable services, etc.  If this exception
   is raised during an update method of an object, it becomes invalid
   and must be recreated.  This would typcially be done by retrieving
   another copy from some process that has it. 

   It is very important that these exceptions be used correctly.
   Using "Error" when "Fatal" should be used will result in copies of
   the objects being out of sync.  Using "Fatal" when "Error" would
   suffice will result in unnecessarily invalidating a copy of the
   object.  Obviously, it is safer to err on the side of raising
   "Fatal" too often rather than too little. *)

VAR                              (*CONST*)
  CommFailure, NetObjFailure, NetObjAlerted, EventFailure, DeadObject,
  Alerted, RecursiveUpdate, IPFailure: Atom.T; 

(* Common exceptions within the shared object runtime system are
   defined here. *)

TYPE
  Callback = EmbProxiedObj.T OBJECT END;

(* A Callback object will be subtyped by the shared object stub
   generator to create a specific Callback object for each specific
   shared data object.  See the example below for the details of
   specific call callback objects. *)

TYPE SequenceNumber = EventNumber.T;
(* The sequence number is the same as an event number.  Each update to
   an object is assigned a sequence number.  Users of the package do
   not currently need to know about sequence numbers. *)

(* \subsection{Controlling Shared Object Behaviour}

   In order to provide control over shared objects, the following
   routines are provided. *)

PROCEDURE Wait(obj: T; c: Thread.Condition; m: Thread.Mutex := NIL);
(* It is important that a thread not needlessly block while inside a
   SharedObj method call.  If a thread wishes to wait on a condition
   variable, it should call "Wait" instead of "Thread.Wait".
   If no Mutex "m" is supplied, the implicit mutex is used.
   The calling thread must have "m" locked if it is
   supplied. *)

PROCEDURE AcquireGlobalLock (obj: T) RAISES {Error, Thread.Alerted};

(* If a client needs to execute a sequence of actions atomically on a
   shared object "obj", the client should first call
   "AcquireGlobalLock(obj)" to acquire a lock on "obj".  Once the lock
   is acquired, it is guaranteed that all updates to "obj" will
   occur atomically until the lock is released. *)

PROCEDURE ReleaseGlobalLock (obj: T) RAISES {Error, Thread.Alerted};

(* When atomicity of updates is no longer required,
   "ReleaseGlobalLock(obj)" will release the lock on "obj" and allow
   other clients to apply updates to "obj". *)

PROCEDURE Own (obj: T; willingness: Timeliness := 0)
  RAISES {Error, Thread.Alerted};

(* As desribed above, update methods normally take a significant
   amount of time to process compared to non-update methods.  In fact,
   the time is comparable to the amount of time taken to process a
   remote object call on a network object.  

   However, if one client will be doing most, or all, of the updating
   of an object, it is not necessary for their update method calls to
   incur this overhead.  The "Own(obj,willingness)" calls declares the local
   process to be the owner of shared object "obj", and the
   "willingness" of the local process to provide direct updates to
   other processes (see the discussion of "Timeliness" below).
   Declaring ownership has a number of side effects:
   \begin{list}
   \item The ordering of update events will be performed in this
   process, significantly reducing the time require for update method
   calls to complete in process.  The time is now comparable to a
   non-update method call.  Unfortunately, the time required for other
   processes to perform update method calls will be increased somewhat.
   \item Acquiring and releasing the global lock is significantly
   faster (analogous to the speed improvement for update method
   calls).  The time required for other processes to acquire and
   release the global lock will be increased somewhat. 
   \item Network traffic may be reduced for updates performed by this object.
   \item The time required for updates made by this process to reach
   certain other copies may be reduced (see the discussion of
   "Timeliness" below).
   \end{list}
*)

PROCEDURE Disown (obj: T) RAISES {Error, Thread.Alerted};

(* When a process that has declared ownership of an object "obj" no longer
   needs, or wishes, to be the owner, "Disown(obj)" reliquishes
   ownership.  *)

TYPE Timeliness = [-8 .. 7];

PROCEDURE SetTimeliness (obj: T; value: Timeliness)
  RAISES {Error, Thread.Alerted};

(* "SetTimeliness(obj, val)" is used to specify to the runtime how
   timely we want updates to "obj" to arrive at our local copy.  The
   default timeliness is 0.  Larger values imply a greater desire for
   timely updates.

   Timeliness values are currently used when some process declares
   itself the owner of "obj".  When declaring ownership, a
   "willingness" value is provided.  Any copy of the object with a
   timeliness greater than this willingness value will receive updates
   directly from the object.  As a result, declaring a timeliness of
   "FIRST(SharedObj.Timeliness)" will prevent the process from ever
   receiving updates directly.  Similarly, declaring a willingness of
   "LAST(SharedObj.Timeliness)" will prevent a process from ever
   sending udpates directly.  Any other combination is obviously
   possible. 

\paragraph{Specials.}

   Specials provide for customized pickling of specified data types on
   every call of "read" or "write" in this process.  See the "Pickle"
   package for the details on "Pickle.Specials".  The Shared Object
   system uses the Pickle package to transmit data between hosts, by
   registering a "Pickle.Special" for each subtype of "SharedObj.T".  A
   "SharedObj.Special" may be registered for a specific subtype of
   "SharedObj.T" and is used by the type's corresponding "Pickle.Special"
   routines to read and write the user defined data for that object.
   By default, all of the user defined data fields of a "SharedObj.T"
   are writen and read.  As with "Pickle.Special"s:

\begin{itemize}
\item  the methods must leave the "Rd.T" or "Wr.T" positioned
       after the last byte read or written;

\item  the "read" method must consume the number of bytes written
       by the "write" method;

\item  the "read" method must produce a value equivalent to the
       one that was given to the "write" method.
\end{itemize}

  If these rules are violated, the result could be either a
  checked runtime error or an invalid result from reading a
  pickle.

  There are many ways to program a special.  For example, 
  the "write" method could modify the value and then call 
  the root special.  Or the "write" method could create a related
  value and call "writer.write" or "Special.write". Or it
  could write some data fields individually and call
  "writer.write" for selected sub-values of its value.  Or it
  could use mixtures of these techniques.

  The SharedObj.Special methods cannot be called themselves.  Each
  SharedObj type will have its own subtype of Special generated, whose
  default methods will read and write the user defined fields of the
  object.  Each type will also have it's own RegisterSpecial(sp)"
  procedure defined, to set the special to be used for that type. 
  *)

TYPE
  Special = OBJECT
    METHODS
      write(obj: T; writer: Pickle.Writer)
        RAISES {Pickle.Error, Wr.Failure, Thread.Alerted};
      read(obj: T; reader: Pickle.Reader)
        RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, 
                Thread.Alerted};
    END;

END SharedObj.

(* \section{Callback Objects}

   For each shared object generated with the shared object stub
   generator, a corresponding callback object is also generated.  The
   details of this object are best explained via an example.

   Assume we have the following shared object definition:

   | TYPE
   |   Data = Logitech.TrackerPosition;
   |   T <: S;
   |   S <: Public;
   |   Public = SharedObj.T OBJECT
   | 		METHODS
   | 		  set (READONLY val: Data) RAISES {SharedObj.Error};
   | 		  get (VAR val: Data) RAISES {SharedObj.Error, Thread.Alerted};
   | 		  <* SHARED UPDATE METHODS set *>
   | 		END;

   The "<* SHARED UPDATE METHODS set *>" pragma declares that the
   "set()" method is an update method.  The following callback object
   will be generated for this shared object:

   | TYPE
   |   T <: Public;
   |   Public = SharedObj.Callback OBJECT 
   |     METHODS
   |       init(obj: TrackerPosition.T): T;
   |       pre_set (READONLY obj: TrackerPosition.T; 
   |                READONLY val: TrackerPosition.Data): BOOLEAN;
   |       pre_anyChange(READONLY obj: TrackerPosition.T);
   |       post_set (READONLY obj: TrackerPosition.T; 
   |                 READONLY val: TrackerPosition.Data): BOOLEAN;
   |       post_anyChange(READONLY obj: TrackerPosition.T);
   |     END;

   There are three kinds of methods in the callback object.  For a
   callback object "cb" and a shared object "obj", the methods of "cb"
   are: 
   \begin{list}
   \item {\it Initialization}.  Calling "cb.init(obj)" initializes cb to be a
   callback for "obj".  When "obj" changes, one or more methods of
   "cb" will be called.
   \item {\it Specific callbacks}.  For each update method in "obj", a
   corresponding pair of methods exist in "cb".  One, the
   "pre_" method, is called prior to the update and the other, the
   "post_" method, is called just after the update.  It is guaranteed
   the object passed to the "pre_" methods represents the state of
   the object just prior to the update.  Similarly, the object passed
   to the "post_" methods represents the state of the object just
   after the update.  The parameters to the
   callback methods are "obj" (the object being updated) and the
   parameters to the corresponding update method.  All of the
   parameters are read only, as it is not permissable to change them
   in the callback methods.  More importantly, it is not permissable
   to call any update methods on "obj" from within a method of "cb".
   The return value is used to indicate if the generic callback should
   be called, as discussed next.
   \item {\it Generic Callback}.  A pair of callbacks,
   "pre_anyChange(obj)" and "post_anyChange(obj)" are called for any
   changes to the object when the corresponding specific callback
   returns "FALSE". 
   \end{list}

   To use the callback objects, the programmer should override those
   methods representing the kinds of change notification desired.  The
   default implementations of the specific callbacks do nothing and
   return "FALSE", causing the generic callback to be involked.  The
   default generic callbacks do nothing.  Therefore, if all that is
   desired is notification that the object has changed, only the
   generic callbacks need to be overridden.  If some combination of
   specific and generic notification is desired, some specific
   callbacks and the generic callbacks can be overridden.  

   When overriding a specific callback, the programmer should
   return "TRUE" to indicate that the generic callback should not be
   called for this update.  It is permissable, however, to return
   "FALSE" and have the generic callback be called if desired.

   Consider the example where we have a tracker object above and we
   wish to noticed the left button on the hypothetical tracker
   changing.  Assume that we "obj.set(val)" is only called if a change
   actually occurs.  Furthermore, assume we can not tell from the
   "val" parameter if the button has changed, and that there is a
   method "obj.getLeftButton()" that will return the button value.  We
   could set up a callback as follows to notify us of the change.

   | TYPE
   |   MyCallback = T OBJECT 
   |       left: BOOLEAN;
   |     OVERRIDES
   |       pre_set := MyPreSet;
   |       post_set := MyPostSet;
   |       post_anyChange := MyPostAnyChange;
   |     END;
   |
   | PROCEDURE MyPreSet(cb: MyCallback; READONLY obj: TrackerPosition.T; 
   |                    READONLY val: TrackerPosition.Data): BOOLEAN =
   |   BEGIN
   |     left := obj.getLeftButton();
   |   END MyPreSet;
   |
   | PROCEDURE MyPostSet(cb: MyCallback; READONLY obj: TrackerPosition.T; 
   |                     READONLY val: TrackerPosition.Data): BOOLEAN =
   |   BEGIN
   |     IF left # obj.getLeftButton() THEN
   |       IO.Put("Left button changed!\n");
   |       RETURN TRUE;
   |     END;
   |     RETURN FALSE;
   |   END MyPostSet;
   | 
   | PROCEDURE MyPostAnyChange(cb: MyCallback; 
   |                           READONLY obj: TrackerPosition.T) =
   |   BEGIN
   |     IO.Put("Something other than the left button changed.\n");
   |   END MyPostAnyChange;

   Now consider an example where we have a similar tracker object and
   we still wish to noticed the left button on the hypothetical tracker
   changing. We could set up a much simpler callback to notify us of
   the change. 

   | TYPE
   |   MyCallback = T OBJECT 
   |     OVERRIDES
   |       pre_set := MyPreSet;
   |       pre_anyChange := MyPostAnyChange;
   |     END;
   |
   | PROCEDURE MyPreSet(cb: MyCallback; READONLY obj: TrackerPosition.T; 
   |                    READONLY val: TrackerPosition.Data): BOOLEAN =
   |   BEGIN
   |     IF val.left_button # obj.getLeftButton() THEN
   |       IO.Put("Left button changed!\n");
   |       RETURN TRUE;
   |     END;
   |     RETURN FALSE;
   |   END MyPreSet;
   |
   | PROCEDURE MyPreAnyChange(cb: MyCallback; 
   |                          READONLY obj: TrackerPosition.T) =
   |   BEGIN
   |     IO.Put("Something other than the left button changed.\n");
   |   END MyPreAnyChange;

   Although contrived, these examples show how to use the callbacks.

*)

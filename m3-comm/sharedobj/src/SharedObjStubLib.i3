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
 * Created On      : Wed May 24 10:28:43 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Fri Nov 22 14:03:15 1996
 * Update Count    : 81
 * 
 * $Source: /opt/cvs/cm3/m3-comm/sharedobj/src/SharedObjStubLib.i3,v $
 * $Date: 2001-12-02 13:14:14 $
 * $Author: wagner $
 * $Revision: 1.1.1.1 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.5  1996/11/22 19:03:19  bm
 * fixed header
 *
 * 
 * HISTORY
 *)

(* For each "SharedObj.T" subtype "T" intended to support the shared
   object protocol, there must be a pair of shared object stubs, one
   of which distributes local method invocations, the other of which
   applies incoming remote method invocations.  \index{Shared Object
   stubs}

   The shared object stub defines a subtype of "T" in which every
   method is overridden by a procedure which sends the method
   invocation information to a central sequencer, waits until it
   receives this information back in sequence with method invocations
   from other copies of the shared object, and then applies the method
   locally.  Such an object is constructed by the shared object
   runtime whenever a subtype "T" of "SharedObj.T" is created and
   initialized, or received via the network object runtime.
   .\index{Shared Object stubs!distribution}

   The shared object stub also defines a single procedure of type
   "Dispatcher" that is called to unmarshal and dispatch remote
   invocations.  \index{Shared Object stubs!incoming method invocation}
*)
INTERFACE SharedObjStubLib;

IMPORT EventProtocol, EventStubLib, Rd, Wr, SharedObj,
       ObjectSpace, Thread, Pickle2 AS Pickle;  
FROM EventProtocol IMPORT Byte8, Int32;

TYPE
  StubProtocol = EventProtocol.StubProtocol;

(*
  Typecode = CARDINAL;
*)
(* "Typecode" is the type of those values returned by the Modula-3
    "TYPECODE" operator. \index{typecodes}
 *)  

  Handle <: PublicHandle;
  PublicHandle = OBJECT
    eh: EventStubLib.Handle;
    local: BOOLEAN;
  END;

(* "Handle" represents a single message being worked on by a client,
   either being created or responded to.  Clients need to see the
   "EventStubLib.Handle" to call the EventStubLib routines. *)

(* \paragraph{Shared object client stubs.}  
\index{stubs!client}

Here is a simplified sketch of the procedure calls performed by a 
client to make a shared update call to a method of "obj":

|  VAR 
|    m := StartCall(obj); 
|  BEGIN
|    IF MarshalArgs(m) THEN
|      <marshal to "c" the number of this method>
|      <marshal to "c" the method arguments>
|      SequenceCall(m, stubProt);
|    END;
|    TRY
|      AcquireWriteLock(obj);
|      <apply the method>
|    FINALLY
|      ReleaseWriteLock(obj);
|      IF m # NIL THEN EndCall(m) END;
|    END
|  END;

The sender always marshals arguments in its native format; the
receiver performs any conversions that may be needed.  Here are the
specifications of the client protocol procedures: *)

PROCEDURE StartCall(obj: SharedObj.T) : Handle
    RAISES {Wr.Failure, Thread.Alerted};
(* Return a Handle structure to the owner of "obj",  
   write to the connection a protocol request to 
   perform a remote method call to "obj", using the 
   data representation "NativeRep".\ttindex{SharedObjRT.StartCall} *)

(* Upon return from "StartCall",  the client stub should marshal 
   a specification of the method being invoked followed by any
   arguments. 
*)
   
PROCEDURE MarshalArgs(m: Handle): BOOLEAN;
(* "MarshalArgs" returns true if the arguments need to be marshalled.
   Arguments are only marshalled if the call is going to be sent to
   other machines. *)

PROCEDURE SequenceCall(m: Handle; stubProt: StubProtocol) 
  RAISES {SharedObj.Error, Thread.Alerted};
(* "SequenceCall" indicates the end of the arguments for the current 
   method invocation event, and blocks waiting for the update 
   event to be sequenced.  The value "stubProt" is the
   stub protocol version under which the arguments and results
   were encoded.\ttindex{SharedObjRT.SequenceCall} *)

(* Upon return from "SequenceCall" the client stub should actually
   apply the method to the local object. *)
    
PROCEDURE EndCall(m: Handle) RAISES {Thread.Alerted};
(* "EndCall" indicates the end of the shared object call.  This call
   must be made after sequence call, and the local method should be
   called between "SequenceCall" and "EndCall".
   \ttindex{SharedObjStubLib.EndCall} *)

(* \paragraph{Marshaling of generic data.} \index{marshaling!of generic
   data} The following procedures are made available to permit the generic
   marshaling of various primitive data types.  In general, the
   "EventStubLib" routines are used, except where equivalent routines
   exist here. *)

PROCEDURE OutRef (h: Handle; r: REFANY) 
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal the data structure reachable from "r".  Certain datatypes are
   handled specially: subtypes of "Rd.T" and "Wr.T" are not
   allowed to be marshalled.  The types "TEXT" and "REF ARRAY OF TEXT" are
   marshaled by copying via custom code for speed.  Subtypes of
   "NetObj.T" and "SharedObj.T" are copied by sending their network
   identifiers and only copying the data at a later point if the
   corresponding object does not exist on the remote machines.   All
   others are marshaled by copying as pickles.
   \ttindex{EventStubLib.OutRef} *) 

PROCEDURE OutChars (h: Handle; READONLY chars: ARRAY OF CHAR)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a char array in native format. *)

PROCEDURE OutBytes (h: Handle; READONLY bytes: ARRAY OF Byte8)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a byte array. *)

PROCEDURE OutInteger (h: Handle; i: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an integer in native format. *)

PROCEDURE OutInt32 (h: Handle; i: Int32) RAISES
  {Wr.Failure, Thread.Alerted}; 
(* Marshal a 32-bit integer in native format. *)

PROCEDURE OutByte (h: Handle; i: Byte8) RAISES
  {Wr.Failure, Thread.Alerted}; 
(* Marshal a byte. *)

PROCEDURE OutBoolean (h: Handle; bool: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a boolean value. *)

PROCEDURE OutReal (h: Handle; r: REAL) RAISES
  {Wr.Failure, Thread.Alerted}; 
(* Marshal a real in native format. *)

PROCEDURE OutLongreal (h: Handle; card: LONGREAL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a longreal in native format. *)

PROCEDURE OutExtended (h: Handle; card: EXTENDED)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an extended in native format. *)

PROCEDURE OutCardinal (h: Handle; card: CARDINAL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a cardinal in native format. *)

(* The following procedures are provided in support of generic unmarshaling
   of data.  In all cases, "rep" indicates the encoding of the incoming
   data.  These procedures could be replaced by inline unmarshaling code
   whenever the relevant elements of "rep" match the corresponding elements
   of "NativeRep". *)

PROCEDURE InRef (h: EventStubLib.Handle; tc := -1): REFANY
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a marshaled subtype of "REFANY" as pickled by "OutRef".  If
   "tc" is non-negative, it is the typecode for the intended type of the
   reference.  A "Error" exception is raised if the unpickled result is not
   a subtype of this type.  If "tc" is negative, no type checking is
   performed.  \ttindex{EventStubLib.InRef} *)

PROCEDURE InChars (h: EventStubLib.Handle; VAR chars: ARRAY OF CHAR)
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a char array of length "NUMBER(chars)". *)

PROCEDURE InBytes (h: EventStubLib.Handle; VAR bytes: ARRAY OF Byte8)
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte array of length "NUMBER(bytes)". *)

PROCEDURE InInteger (h: EventStubLib.Handle;
                     min            := FIRST(INTEGER);
                     max            := LAST(INTEGER)   ): INTEGER
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an integer, checking that its value is in "[min..max]". *)

PROCEDURE InInt32 (h: EventStubLib.Handle; min := FIRST(Int32); max :=
  LAST(Int32)): Int32 RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a 32-bit integer, checking that its value is in
   "[min..max]". *)

PROCEDURE InByte (h: EventStubLib.Handle; max := LAST(Byte8)): Byte8
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte, checking that its value is in "[0..max]". *)

PROCEDURE InBoolean (h: EventStubLib.Handle): BOOLEAN
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a boolean value. *)

PROCEDURE InReal (h: EventStubLib.Handle): REAL
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a real value. *)

PROCEDURE InLongreal (h: EventStubLib.Handle): LONGREAL
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a longreal value. *)

PROCEDURE InExtended (h: EventStubLib.Handle): EXTENDED
  RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an extended value. *)

PROCEDURE InCardinal (h: EventStubLib.Handle; 
                      lim: CARDINAL := LAST(CARDINAL)): CARDINAL
                      RAISES {SharedObj.Error, Rd.Failure, Thread.Alerted}; 
(* Unmarshal a cardinal, checking that its value is in "[0..lim]". *)

(* \paragraph{Local Object Locking.}  
   \index{Shared object stubs!locking the object}
   The stubs need to get lock the object before they act on it.
   Non-update method stubs execute the object method inside a read lock, which
   can be acquired simultaneously be multiple readers, as follows:

|  TRY
|    AcquireReadLock(self);
|    RETURN <execute non-update method>
|  FINALLY
|    ReleaseReadLock(self);
|  END;

   Update methods execute the object method inside an exclusive lock,
   in a similar fashion:

|  TRY
|    AcquireWriteLock(self);
|    RETURN <execute update method>
|  FINALLY
|    ReleaseWriteLock(self);
|  END;
*)

PROCEDURE ReleaseReadLock(self: SharedObj.T);
PROCEDURE AcquireReadLock(self: SharedObj.T);
PROCEDURE ReleaseWriteLock(self: SharedObj.T);
PROCEDURE AcquireWriteLock(self: SharedObj.T);

(* \paragraph{Pickling stub routines.}
   Before an object can be sent to another machine, the local space
   must finish initializing.  In particular, we must either identify
   ourself as a sequencer or set our sequencer.  *)

PROCEDURE StartWritePickle(obj: SharedObj.T; wr: Pickle.Writer) 
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted};

PROCEDURE EndWritePickle(obj: SharedObj.T; wr: Pickle.Writer) 
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted};

PROCEDURE StartReadPickle(obj: SharedObj.T; rd: Pickle.Reader; 
                          from: ObjectSpace.T)
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};

PROCEDURE SetupNewCopy(obj: SharedObj.T; rd: Pickle.Reader; id: Pickle.RefID; 
                       from: ObjectSpace.T): SharedObj.T
  RAISES {Pickle.Error, Thread.Alerted};

PROCEDURE InhibitTransmission(tc: INTEGER; reason: TEXT);
  (* Inhibits the network transmission of any object whose type has
     typecode "tc"; a Pickle.Error will result. By default, Callback
     objects cannot be transmitted.  If an implementor wishes to
     transmit a Callback, they must implement the pickle function.
     The reason should be a string like "callback object cannot
     be transmitted/duplicated". *)

END SharedObjStubLib.

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
 * Created On      : Sat Apr 15 13:56:49 1995
 * Last Modified By: Blair MacIntyre
 * Last Modified On: Thu Nov 21 17:45:48 1996
 * Update Count    : 87
 * 
 * $Source: /opt/cvs/cm3/m3-comm/events/src/EventStubLib.i3,v $
 * $Date: 2001-12-02 00:20:37 $
 * $Author: wagner $
 * $Revision: 1.2 $
 * 
 * $Log: not supported by cvs2svn $
 * Revision 1.1.1.1  2001/12/02 00:06:45  wagner
 * Blair MacIntyre's events library
 *
 * Revision 1.4  1996/11/21 22:45:53  bm
 * fixed header
 *
 * 
 * HISTORY
 *   Based on StubLib.i3, from the network objects package. *)
(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* StubLib.i3 *)
(* Last modified on Thu Feb 24 17:36:24 PST 1994 by wobber *)
(* modified on Fri Feb 5 10:14:33 PST 1993 by owicki *)
(* modified on Tue Dec 8 10:22:26 1992 by gnelson *)

(* This interface contains procedures to be used by stub code for
   distributing events, such as shared object methods in the Shared Object
   package.  It packs a network representation of the event in a reader *)

INTERFACE EventStubLib;

IMPORT Atom, EventProtocol, Rd, Wr, Thread, Event, EventNumber,
       Pickle2 AS Pickle;

FROM EventProtocol IMPORT Byte8, Int32, StubProtocol;

TYPE Handle <: ROOT;

(* Events are created via a object of type "Handle", which is opaque in this
   interface.  The "EventHandle" interface reveals more of this type's
   structure to clients who wish to hand-code stubs for efficiency.

   A "Handle" is unmonitored: clients must not access it from two threads
   concurrently.  \ttindex{EventStubLib.Handle}.

   Stubs may optimize in-line unmarshaling by first checking that the
   incoming representation is the same as the native one for all data types
   relevant to the call.  If it is not, then the generic data unmarshaling
   routines at the end of this interface should be used.

   Automatic conversion between the data representations is performed
   wherever possible.  If automatic conversion is impossible, a "Error"
   exception is raised.

   Concrete values for the elements of "DataRep" are not defined here as it
   is sufficient to compare against "NativeRep" and invoke the marshaling
   procedures defined below if the encoding is non-native. *)

CONST
  NullStubProtocol   = -1;
  SystemStubProtocol = 0;

(* "NullStubProtocol" is a placeholder to indicate the absence of a stub
   protocol value.  "SystemStubProtocol" indicates the fixed stub encoding
   used by the runtime to implement primitives that operate prior to any
   version negotiation.  \ttindex{EventStubLib.NullStubProtocol}
   \ttindex{EventStubLib.SystemStubProtocol} *)

VAR (*CONST*) UnmarshalFailure, UnsupportedDataRep: Atom.T;

(* "Event.Error" is raised to indicate problems with event marshalling or
   unmarshalling.\ttindex{EventStubLib.Error} 
   "UnmarshalFailure" should be used as an argument to "Error" whenever
   stubs encounter a network datum that is incompatible with the target
   type.  For example, the stub code might encounter a "CARDINAL" greater
   than "LAST(CARDINAL)" or an unrecognized remote method specification.
   "UnsupportedDataRep" indicates a mismatch between the network
   representation of data and the ability of a receiver to handle it, for
   example a 64-bit "INTEGER" with non-zero high-order bits is not
   meaningful as an "INTEGER" on a 32-bit machine. *)

(*
\paragraph{Event stub procedures.}
\index{Event stubs!distribution}

Here is a simplified sketch of the procedure calls performed by a
client to create an event:

|  VAR
|    h: Handle;
|    event: Event.T;
|    seq: EventNumber.T;
|  BEGIN
|    TRY
|      h := StartCreate();
|      <marshal to "h" the event data>
|      event := EndCreate(h, id, stubProt, seq);
|      <do something with event, such as send it off to somewhere>
|      Event.Free(event);
|    FINALLY
|      <handle exceptions>
|    END
|  END;

The sender always marshals values in its native format; the receiver
performs any conversions that may be needed.

Here is a simplified sketch of the procedure calls performed by a
client to read an event:

|  VAR
|    h: Handle;
|  BEGIN
|    TRY
|      h := StartRead(ev);
|      <unmarshal the event data from "h">
|      EndRead(h);
|      <do something with event data>
|    FINALLY
|      <handle exceptions>
|    END
|  END;

Here are the specifications of the client protocol procedures: *)

PROCEDURE StartCreate (): Handle;

(* Return a handle to the owner of "obj" which will be used to create
   an event using the data representation "NativeRep".  
   \ttindex{EventStubLib.StartCreate} *)

(* Upon return from "StartCreate", the client stub should marshal the event
   data. *)

PROCEDURE EndCreate (h: Handle; id: Byte8; stubProt: StubProtocol; 
                       num: EventNumber.T): Event.T;

(* "EndCreate" must be called at the end of marshalling an event for
   distribution.\ttindex{EventStubLib.End} "EndCreate" then releases
   "h" and initializes annd returns an "Event.T" containing the event.
   The value "stubProt" is the stub protocol version under which the
   event data was encoded.  The value "id" is the identifier of the event
   type, used to invoke the dispatch routines for event delivery.
   "num" is the initial number of the event, which can be changed
   after the event is created with "ChangeNumber()" below.
   After "EndCreate" returns, "h" should not be
   used.  \ttindex{EventStubLib.EndCreate} *)

PROCEDURE StartRead (ev: Event.T): Handle
  RAISES {Rd.Failure, Thread.Alerted};

(* Return a handle to the owner of "obj" for reading data from "ev".
   \ttindex{EventStubLib.StartRead} *)

(* Upon return from "StartRead", the client stub should unmarshal the event
   data. *)

PROCEDURE EndRead (h: Handle);

(* "EndRead" must be called at the end of unmarshalling an 
   event.\ttindex{EventStubLib.End} "EndRead" then releases "h" and
   returns an "Event.T" containing the event.  After "EndRead" returns, "h"
   should not be used.\ttindex{EventStubLib.EndRead} *)

PROCEDURE ChangeNumber(ev: Event.T; en: EventNumber.T);

(* "ChangeNumber" is called to change the number of an event. *)

(* \paragraph{Marshaling of reference types.} \index{marshaling!of
   reference types} The following procedures are made available for
   marshaling of subtypes of "REFANY". *)

PROCEDURE OutRef (h: Handle; r: REFANY) RAISES {Wr.Failure, Thread.Alerted};
(* Marshal the data structure reachable from "r".  Certain datatypes are
   handled specially: subtypes of "NetObj.T", "Rd.T" and "Wr.T" are not
   allowed to be marshalled.  The types "TEXT" and "REF ARRAY OF TEXT" are
   marshaled by copying via custom code for speed.  All others are
   marshaled by copying as pickles.  \ttindex{EventStubLib.OutRef} *)

PROCEDURE InRef (h: Handle; tc := -1): REFANY
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a marshaled subtype of "REFANY" as pickled by "OutRef".  If
   "tc" is non-negative, it is the typecode for the intended type of the
   reference.  A "Error" exception is raised if the unpickled result is not
   a subtype of this type.  If "tc" is negative, no type checking is
   performed.  \ttindex{EventStubLib.InRef} *)

(* \paragraph{Marshaling of generic data.} \index{marshaling!of generic
   data} The following procedures are made available to permit the generic
   marshaling of various primitive data types. *)

PROCEDURE OutChars (h: Handle; READONLY chars: ARRAY OF CHAR)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a char array in native format. *)

PROCEDURE OutBytes (h: Handle; READONLY bytes: ARRAY OF Byte8)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a byte array. *)

PROCEDURE OutInteger (h: Handle; i: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an integer in native format. *)

PROCEDURE OutInt32 (h: Handle; i: Int32) RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a 32-bit integer in native format. *)

PROCEDURE OutByte (h: Handle; i: Byte8) RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a byte. *)

PROCEDURE OutBoolean (h: Handle; bool: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a boolean value. *)

PROCEDURE OutReal (h: Handle; r: REAL) RAISES {Wr.Failure, Thread.Alerted};
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

PROCEDURE OutEventNumber (h: Handle; n: EventNumber.T)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an EventNumber.T in native format. *)

(* The following procedures are provided in support of generic unmarshaling
   of data.  In all cases, "rep" indicates the encoding of the incoming
   data.  These procedures could be replaced by inline unmarshaling code
   whenever the relevant elements of "rep" match the corresponding elements
   of "NativeRep". *)

PROCEDURE InChars (h: Handle; VAR chars: ARRAY OF CHAR)
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a char array of length "NUMBER(chars)". *)

PROCEDURE InBytes (h: Handle; VAR bytes: ARRAY OF Byte8)
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte array of length "NUMBER(bytes)". *)

PROCEDURE InInteger (h: Handle;
                     min            := FIRST(INTEGER);
                     max            := LAST(INTEGER)   ): INTEGER
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an integer, checking that its value is in "[min..max]". *)

PROCEDURE InInt32 (h: Handle; min := FIRST(Int32); max := LAST(Int32)):
  Int32 RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a 32-bit integer, checking that its value is in
   "[min..max]". *)

PROCEDURE InByte (h: Handle; max := LAST(Byte8)): Byte8
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte, checking that its value is in "[0..max]". *)

PROCEDURE InBoolean (h: Handle): BOOLEAN
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a boolean value. *)

PROCEDURE InReal (h: Handle): REAL
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a real value. *)

PROCEDURE InLongreal (h: Handle): LONGREAL
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a longreal value. *)

PROCEDURE InExtended (h: Handle): EXTENDED
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an extended value. *)

PROCEDURE InCardinal (h: Handle; lim: CARDINAL := LAST(CARDINAL)):
  CARDINAL RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a cardinal, checking that its value is in "[0..lim]". *)

PROCEDURE InEventNumber (h: Handle): EventNumber.T
  RAISES {Event.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an EventNumber.T in native format. *)


(* \smallskip

   Here are some procedures for raising "Event.Error" exceptions conveniently:

   \smallskip *)

PROCEDURE RaiseUnmarshalFailure () RAISES {Event.Error};
(* Raise "Event.Error(AtomList.List1(UnmarshalFailure))". *)

PROCEDURE RaiseUnsupportedDataRep () RAISES {Event.Error};
(* Raise "Event.Error(AtomList.List1(UnsupportedDataRep))". *)


(* \smallskip

   Here are some procedures for raising writing picklers.  Using this
   routines, you can check to see if the current pickle reader or
   writer is reading from or writing to an Event.

   \smallskip *)

PROCEDURE IsEventWriter(wr: Pickle.Writer): BOOLEAN;
PROCEDURE IsEventReader(wr: Pickle.Reader): BOOLEAN;

END EventStubLib.

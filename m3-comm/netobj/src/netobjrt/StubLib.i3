(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StubLib.i3 *)
(* Last modified on Thu Feb 24 17:36:24 PST 1994 by wobber      *)
(*      modified on Fri Feb  5 10:14:33 PST 1993 by owicki      *)
(*      modified on Tue Dec  8 10:22:26 1992 by gnelson     *)

(* This interface contains procedures to be used by stub code for
   invoking remote object methods and servicing remote invocations.

   Each stub module provides type-dependent network 
   support for marshaling and unmarshaling method calls for a 
   specific subtype of "NetObj.T".  Usually, stubs are 
   built automatically.\ttindex{NetObj.T}

   For each "NetObj.T" subtype "T" intended to support remote method 
   invocation there must be both a client and a server stub. 
   \index{stubs}

   The client stub defines a subtype of "T" in which every method is 
   overridden by a procedure implementing remote method invocation. 
   Such a {\it surrogate} object is constructed by the network 
   object runtime whenever a reference to a non-local object 
   is encountered.\index{stubs!client} 

   The server stub consists of a single procedure of type "Dispatcher"
   that is called to unmarshal and dispatch remote invocations.
   \index{stubs!server}

   A surrogate type and null dispatcher for "NetObj.T" are defined and
   registered by the network object system itself. *)

INTERFACE StubLib;

IMPORT Atom, AtomList, NetObj, Rd, Wr, Thread;

TYPE
  Conn <: ROOT;

(* A remote object invocation can be viewed as an exchange of messages 
   between client and server.  The messages are exchanged via
   an object of type "Conn", which is opaque in this interface.
   The "StubConn" interface reveals more of this type's structure
   to clients who wish to hand-code stubs for efficiency.

   A "Conn" is unmonitored: clients must not access it from two threads
   concurrently. \ttindex{StubLib.Conn} *)

TYPE
  Byte8 = BITS 8 FOR [0..255];
  DataRep = RECORD
    private, intFmt, floatFmt, charSet: Byte8;
  END;

VAR (*CONST*) NativeRep: DataRep;

(* The type "DataRep" describes the format used to encode characters, 
   integers, and floating point numbers in network data.  Data is 
   always marshaled in the sender's native format.  "NativeRep" is 
   a runtime constant that describes the native format of the current 
   environment.\ttindex{StubLib.DataRep}\ttindex{StubLib.NativeRep}
   
   Stubs may optimize in-line unmarshaling by first checking that the 
   incoming representation is the same as the native one for all data types
   relevant to the call.  If it is not, then the generic data unmarshaling
   routines at the end of this interface should be used.

   Automatic conversion between the data representations is performed wherever
   possible.  If automatic conversion is impossible, the the exception
   "NetObj.Error(NetObj.UnsupportedDataRep)" is raised.

   Concrete values for the elements of "DataRep" are not defined here
   as it is sufficient to compare against "NativeRep" and invoke the
   marshaling procedures defined below if the encoding is non-native. *)

TYPE
  Int32 =  BITS 32 FOR [-16_7FFFFFFF-1..16_7FFFFFFF];
  StubProtocol = Int32;

CONST
  NullStubProtocol = -1;
  SystemStubProtocol = 0;

(* The type "StubProtocol" indicates the version of the stub compiler
   used to generate a particular stub.  Multiple stubs for the same
   network object can coexist within the same program (for example, 
   the outputs of different stub compilers).  During surrogate creation,
   the network object runtime negotiates the stub protocol version
   with the object owner.\ttindex{StubLib.StubProtocol}

   "NullStubProtocol" is a placeholder to indicate the absence of
   a stub protocol value.  "SystemStubProtocol" indicates the fixed
   stub encoding used by the runtime to implement primitives
   that operate prior to any version negotiation.
   \ttindex{StubLib.NullStubProtocol}\ttindex{StubLib.SystemStubProtocol} *)

VAR (*CONST*) UnmarshalFailure: Atom.T;

(* "UnmarshalFailure" should be used as an argument to "NetObj.Error"
   whenever stubs encounter a network datum that is incompatible with
   the target type.  For example, the stub code might encounter a
   "CARDINAL" greater than "LAST(CARDINAL)" or an unrecognized remote
   method specification. *)

TYPE Typecode = CARDINAL;
  
(* "Typecode" is the type of those values returned by the Modula-3
    "TYPECODE" operator. \index{typecodes}
 *)  
    
PROCEDURE Register(
    pureTC: Typecode;
    stubProt: StubProtocol;
    surrTC: Typecode;
    disp: Dispatcher);
(* Let "T" be the type whose typecode is "pureTC", and
   let "srgT" be the type whose typecode is "surrTC".
   Set the client surrogate type and dispatch procedure
   for "T" to be "srgT" and "disp", respectively.
   The "stubProt" parameter indicates the stub compiler
   version that generated the stub being registered.
   \ttindex{StubLib.Register}  *)

(* The following constraint applies to stub registration.
   If stubs are registered for types "A" and "B", where "B" is 
   a supertype of "A", then the protocol versions registered for 
   "B" must be a superset of the versions registered for "A". 
   If this rule is violated, attempts to invoke remote methods
   may raise "NetObj.Error".
   
   Note that a concrete object of type "A" will receive method 
   invocations only for stub versions for which "A" is registered.  
   This is true even if a supertype of "A" is registered with 
   additional stub versions.

   "Register" must be called before any object of type "T" is 
   marshaled or unmarshaled.  *)

(*
\paragraph{Client stub procedures.}
\index{stubs!client}

Here is a simplified sketch of the procedure calls performed by a 
client to make a remote call to a method of "obj":

|  VAR 
|    c := StartCall(obj, stubProt); 
|    resDataRep: DataRep;
|  BEGIN
|    <marshal to "c" the number of this method>
|    <marshal to "c" the method arguments>
|    resDataRep := AwaitResult(conn);
|    <unmarshal from "c" the method results>
|    <results will be in wire format "resDataRep">
|    EndCall(c, TRUE)
|  END;

For both arguments and results, the sender always marshals values 
in its native format; the receiver performs any conversions that may 
be needed.  The procedure result typically begins with an integer 
specifying either a normal return or an exceptional return.  If a 
protocol error occurs, the client should call "EndCall(c, FALSE)" 
instead of "EndCall(c, TRUE)". This requires "TRY FINALLY" instead 
of the simple straight-line code above; a more complete example is 
presented in the next section. 

Here are the specifications of the client protocol procedures: *)

PROCEDURE StartCall(obj: NetObj.T;
                    stubProt: StubProtocol) : Conn
    RAISES {NetObj.Error, Wr.Failure, Thread.Alerted};
(* Return a connection to the owner of "obj",  
   write to the connection a protocol request to 
   perform a remote method call to "obj", using the 
   data representation "NativeRep".  The value "stubProt" is the
   stub protocol version under which the arguments and results
   will be encoded.\ttindex{StubLib.StartCall} *)

(* Upon return from "StartCall",  the client stub should marshal 
   a specification of the method being invoked followed by any arguments. *)
   
PROCEDURE AwaitResult(c: Conn): DataRep
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
            Thread.Alerted};
(* "AwaitResult" indicates the end of the arguments for the current 
    method invocation, and blocks waiting for a reply message
    containing the result of the invocation.  It returns
    the data representation used to encode the result
    message.\ttindex{StubLib.AwaitResult} *)

(*  Upon return from "AwaitResult" the client stub should unmarshal
    any results. *)
    
PROCEDURE EndCall(c: Conn; reUse: BOOLEAN)
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure,
            Thread.Alerted};
(* "EndCall" must be called at the end of processing a remote 
   invocation, whether or not the invocation raised an exception. 
   The argument "reUse" must be "FALSE" if the client has been 
   unable, for any reason, to unmarshal either a 
   normal or exceptional result. It is always safe to 
   call "EndCall" with "reUse" set to "FALSE", but performance 
   will be improved if "reUse" is "TRUE" whenever possible.
   \ttindex{StubLib.EndCall} *)
   
(* "EndCall" determines, by examining "c", whether the result message 
   requires acknowledgment, that is, whether the result contained 
   any network objects.  If an acknowledgment is required, it is sent.  
   "EndCall" then releases "c".  After "EndCall" returns, "c" should not 
   be used.

\paragraph{Server dispatcher procedures.}  
\index{stubs!server}\index{dispatching}
Next we consider the server-side stub, which consists of
a registered dispatcher procedure.
*)

TYPE 
  Dispatcher = 
    PROCEDURE(c: Conn; obj: NetObj.T; 
              rep: DataRep; stubProt: StubProtocol)
    RAISES {NetObj.Error, Rd.Failure, Wr.Failure, 
            Thread.Alerted};

(* A procedure of type "Dispatcher" is registered for each network
   object type "T" for which stubs exist.  The dispatcher 
   is called by the network object runtime when it receives a remote
   object invocation for an object of type "T".  The "rep" argument
   indicates the data representation used to encode the arguments of the
   invocation.  The "stubProt" argument indicates the version of
   stub protocol used to encode the call arguments.  The same protocol
   should be used to encode any results.\ttindex{StubLib.Dispatcher}
   
   The dispatcher procedure is responsible for unmarshaling the method
   number and any arguments, invoking the concrete object's
   method, and marshaling any results.
   
   Here is a simplified sketch of a typical dispatcher:

| PROCEDURE Dispatch(c, obj, rep) =
|    <unmarshal from "c" the method number>
|    <unmarshal from "c" the method arguments>
|    <arguments will be in the wire format "rep")>
|    <call the appropriate method of "obj">
|    StartResult(c);
|    <marshal to "c" the method result or exception>
|  END Dispatch;

Here is the specification of "StartResult": *)

PROCEDURE StartResult(c: Conn) 
    RAISES {Wr.Failure, Thread.Alerted};
(* "StartResult" must be called by the server stub to initiate return 
   from a remote invocation before marshaling any results.
   \ttindex{StubLib.StartResult} *)  
   
(* Upon return from "StartResult" the stub 
   code should marshal any results or error indications on "c". 
 
\paragraph{Marshaling of reference types.}  
\index{marshaling!of reference types}
  The following procedures are made available for marshaling of 
  subtypes of "REFANY". *)

PROCEDURE OutRef(c: Conn; r: REFANY)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal the data structure reachable from "r".  Certain datatypes
   are handled specially: subtypes of "NetObj.T" are
   marshaled as network references.  Subtypes of "Rd.T" and "Wr.T"
   are marshaled as surrogate streams.  The types "TEXT" and "REF ARRAY
   OF TEXT" are marshaled by copying via
   custom code for speed.  All others are marshaled by copying as pickles.
   Subtypes of "NetObj.T", "Rd.T", and "Wr.T" which are embedded within
   other datatypes are also marshaled by reference.
   \ttindex{StubLib.OutRef} *)

PROCEDURE InRef(c: Conn; rep: DataRep; tc:=-1): REFANY
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a marshaled subtype of "REFANY" as pickled by "OutRef".
   If "tc" is non-negative, it is the typecode for the intended 
   type of the reference.  The exception "NetObj.Error(UnmarshalFailure)" 
   is raised if the unpickled result is not a subtype of this type.  If
   "tc" is negative, no type checking is performed.
   \ttindex{StubLib.InRef} *)

(* For any subtypes of "NetObj.T" in the pickled datatype, a surrogate
   network object is substituted for the concrete object.  The runtime
   guarantees that within a single address space, only one surrogate
   for any single concrete object will exist at any given time.

   For any unmarshaled subtypes of "Rd.T", the concrete reader
   is replaced by a surrogate reader whose source is the remaining
   source of the concrete reader.  Similarly for subtypes of "Wr.T",
   the concrete writer is replaced by a surrogate writer whose target
   is the target of the concrete writer.  The operations on
   surrogate streams are described in "NetStream.i3".
   
\paragraph{Marshaling of generic data.}  
\index{marshaling!of generic data}
  The following procedures are made available to permit the generic
  marshaling of various primitive data types. *)

PROCEDURE OutChars(
    c: Conn; READONLY chars: ARRAY OF CHAR)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a char array in native format. *)

PROCEDURE OutBytes(
    c: Conn; READONLY bytes: ARRAY OF Byte8)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a byte array. *)

PROCEDURE OutInteger(c: Conn; i: INTEGER)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an integer in native format. *)

PROCEDURE OutInt32(c: Conn; i: Int32)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a 32-bit integer in native format. *)

PROCEDURE OutByte(c: Conn; i: Byte8)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a byte. *)

PROCEDURE OutBoolean(c: Conn; bool: BOOLEAN)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a boolean value. *)

PROCEDURE OutReal(c: Conn; r: REAL)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a real in native format. *)

PROCEDURE OutLongreal(c: Conn; card: LONGREAL)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a longreal in native format. *)

PROCEDURE OutExtended(c: Conn; card: EXTENDED)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an extended in native format. *)

PROCEDURE OutCardinal(c: Conn; card: CARDINAL)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a cardinal in native format. *)


(* The following procedures are provided in support of generic 
   unmarshaling of data.  In all cases, "rep" indicates the encoding
   of the incoming data.  These procedures could be replaced 
   by inline unmarshaling code whenever the relevant elements of
   "rep" match the corresponding elements of "NativeRep". *)

PROCEDURE InChars(
    c: Conn; rep: DataRep;
    VAR chars: ARRAY OF CHAR)
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a char array of length "NUMBER(chars)". *)

PROCEDURE InBytes(
    c: Conn; VAR bytes: ARRAY OF Byte8)
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte array of length "NUMBER(bytes)". *)

PROCEDURE InInteger(
    c: Conn; 
    rep: DataRep;
    min := FIRST(INTEGER);
    max := LAST(INTEGER)): INTEGER
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an integer, checking that its value is in  "[min..max]". *)

PROCEDURE InInt32(
    c: Conn; 
    rep: DataRep;
    min := FIRST(Int32);
    max := LAST(Int32)): Int32
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a 32-bit integer, checking that its value is in  "[min..max]". *)

PROCEDURE InByte(
    c: Conn; 
    max := LAST(Byte8)): Byte8
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte, checking that its value is in "[0..max]". *)

PROCEDURE InBoolean(c: Conn): BOOLEAN
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a boolean value. *)

PROCEDURE InReal(c: Conn; rep: DataRep): REAL
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a real value. *)

PROCEDURE InLongreal(c: Conn; rep: DataRep): LONGREAL
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a longreal value. *)

PROCEDURE InExtended(c: Conn; rep: DataRep): EXTENDED
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an extended value. *)

PROCEDURE InCardinal(
    c: Conn; rep: DataRep;
    lim: CARDINAL := LAST(CARDINAL)): CARDINAL
    RAISES {NetObj.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a cardinal, checking that its value is in "[0..lim]". *)


(* \smallskip

   Here are two procedures for raising "NetObj" exceptions conveniently:

   \smallskip
*)

PROCEDURE RaiseUnmarshalFailure()
    RAISES {NetObj.Error};
(* Raise "NetObj.Error(AtomList.List1(UnmarshalFailure))". *)

PROCEDURE RaiseCommFailure(ec: AtomList.T)
    RAISES {NetObj.Error};
(* Raise "NetObj.Error(AtomList.Cons(NetObj.CommFailure, ec))". *)

END StubLib.

(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* SpecialObj.i3 *)
(* Last modified on Fri Apr  9 11:22:56 PDT 1993 by wobber *)
(*      modified on Wed Sep  2 13:12:53 PDT 1992 by evers  *)
(*      modified on Wed Jun 10 16:57:23 PDT 1992 by owicki *)

(* The "SpecialObj" interface defines the network object runtime {\it 
   special object}.  This object provides methods for maintenance 
   of the per-space agent export table (see "NetObj.i3") as well as 
   methods to support object marshalling and garbage collection.  
   There is one such concrete object per address space instance.  
   In addition, there are potentially many special object surrogates 
   used to invoke corresponding methods in different address spaces. *)
   
INTERFACE SpecialObj;

IMPORT NetObj, Fingerprint, SpaceID, StubLib, Transport, Thread, WireRep;

TYPE
  FpTower = REF ARRAY OF Fingerprint.T;

(* A "Fingerprint" is a hashed representation of a Modula-3 type. 
   Every network object type can be represented by such a hash. The 
   hash function must yield uniform results across all network object 
   clients.  An "FpTower" is an ordered list of "Fingerprint" which 
   represents the type hierarchy for a single network object.  Each list 
   is ordered from subtype to supertype.  The fingerprint of the common
   supertype NetObj.T is omitted, but implied. *)

  VersionList = ARRAY OF StubLib.StubProtocol;

(* A "VersionList" is an array of "StubLib.StubProtocol".  For any given
   surrogate type, there may be multiple stub instances corresponding to
   compilations under different stub compilers.  Similarly, an object
   owner may support dispatchers for several stub protocol version.

   Items of the type "FpTower" are always accompanied by a "VersionList".
   The general scheme for attributing stub versions to the elements of a
   "FpTower" is as follows:

       let "fp" be a "FpTower";
       let "ver" be a set of pairs
               "(StubLib.StubProtocol,INTEGER)";

       for any element "ver(v,n)":
           version "v" stubs exist for "fp[i]" where "n <= i"

   We adopt a more restrictive scheme where "ver" is simply an
   array of "StubLib.StubProtocol".  This corresponds exactly to
   the general form where "n" is always zero.

   The effect of this is that the stub versions appearing in "ver"
   apply to all the elements of "fp".  However, stub versions that don't
   exist for "fp[0]" will not appear in the representation.

   For example, if a server has stubs for "(A, v1)" and "(AB, v2)", then
   then "v1" stubs will not be accessible for "AB" objects.

   For a version list "ver", if "ver[i] = StubLib.NullStubProtocol", then
   "ver[j] = StubLib.NullStubProtocol" where j >= i.
*)

  EventID = ARRAY [0..1] OF StubLib.Int32; (* lsw..msw *)
  
(* An "EventID" is a monotonically increasing value which identifies 
   events communicated between a client's garbage collector and the 
   object owners for which that client hold surrogates.  This value 
   serves to serialize calls which otherwise might arrive and be 
   processed asynchronously. *)

  CleanElem = RECORD wrep: WireRep.T; id: EventID; END;
  CleanBatch = ARRAY OF CleanElem;

(* A "CleanBatch" is a list of wire representations which refer to
   network objects to be marked clean.  It is used only in the
   "ST.clean: call below.  Each "wrep" is tagged with an "EventID"
   to logically serialize calls to "dirty" and "clean" according to
   the order in which they were generated. *)
   
CONST DefaultBatchLen = 50;

TYPE
  ST = NetObj.T OBJECT METHODS
    get(name: TEXT) : NetObj.T
      RAISES {NetObj.Error, Thread.Alerted};
    put(name: TEXT; obj: NetObj.T)
      RAISES {NetObj.Error, Thread.Alerted};
    getAdr(sp: SpaceID.T): NetObj.Address
      RAISES {NetObj.Error, Thread.Alerted};
    dirty(
      wrep: WireRep.T; eventID: EventID;
      VAR (*OUT*) vers: VersionList;
      loc: Transport.Location := NIL): FpTower
      RAISES {NetObj.Error, Thread.Alerted};
    clean(
      READONLY batch: CleanBatch; strong: BOOLEAN := FALSE;
      loc: Transport.Location := NIL)
      RAISES {NetObj.Error, Thread.Alerted};
  END;
  
(* The special object "ST" for each address space instance implements
   an agent table describing a set of exported objects.  The "get"
   method performs a table lookup and returns the object registered
   under "name", or "NIL" is there isn't one.  The "put" method
   registers "obj" in the table under "name", overwriting any existing
   entry.

   Each address space contains a table holding a "NetObj.Address" 
   for itself and for every other address space for which it has any
   surrogates.  "getAdr" performs a lookup in this table and returns
   the result.  This is used by the object unmarshalling code to
   construct a "Transport.Location" to the object owner if no such
   location exists.
   
   The "dirty" method is used to inform the object owner of the 
   existence of a surrogate with wire representation "wrep" at the 
   address space whose local garbage collector is identified by 
   "loc".  The caller must tag each call with a unique and 
   monotonically increasing "eventID".  Dirty calls must possess a 
   more recent "eventID" than the most recent one attributed to "wrep" 
   and "loc" by the object owner.  Otherwise the call is considered 
   out-of-date and is ignored.  The "loc" argument is always defaulted 
   during surrogate invocations and is supplied by the server stub 
   during concrete method invocation.  "loc" is required so that the 
   liveness of the surrogate's address space can be monitored.

   The "dirty" call uses the out parameter "vers" to return a subset of
   the stub protocol versions supported by the object owner for the
   object class of "wrep".  If the result array is too small, the owner
   will choose which elements to omit.  If the result array is too large,
   empty slots will be filled with the value "StubLib.NullStubProtocol".
   
   The "clean" method notifies an object owner that a surrogate for
   "wrep" no longer exists at "loc".  The "batch" argument is an
   array of ["wrep", "eventID"] tuples.  As with "dirty", each
   "wrep", is tagged with an "eventID", and this "eventID" must be
   the most recent id received by the owner for "wrep" and "loc". 
   The "loc" argument is always defaulted during surrogate invocations
   and is supplied by the server stub during concrete method invocation.
   If "clean" fails due to communication problems, it must be retried
   with "strong = TRUE" until it is determined that such attempts will
   never succeed, for instance because it is determined that the communication
   problem is permanent.
 *)

TYPE Surrogate <: ST;
  
(* "Surrogate" is the type of surrogate special objects. *)

PROCEDURE New(loc: Transport.Location) : ST;

(* "New" returns a surrogate for the concrete special object at the
   target address space of "loc". *)

END SpecialObj.


(* $Id$ *)

GENERIC INTERFACE SXProxy(Elem, SXElemType);
IMPORT VarUI;
IMPORT SX;
IMPORT SXConversion;

(* replacement for Proxys defined in VarUI.i3.
   
   The revelations work as follows:

   VarUI.VarProxy

   XXX the names here are out of date, but the general structure is correct

   Public             ---    this generic interface
   S                  ---    the corresponding generic module
   T                  ---    SXProxy.m3 (full revelations)

   Type-conversion code is manually added to T, but everything else
   is generic in S.

   The existence of S could be revealed elsewhere, but it needs to be
   shared between SXProxy.m3 and SXProxy.mg, so we save an interface by
   putting S here.  Just don't allocate it, please.

*)

(* Notes on locking:
   We do NOT acquire the lock on t.mu before writing it under any 
   circumstances.  We depend on the locking done by the SX implementation,
   which is sufficient for correct memory accesses.  

   Therefore, users of this interface cannot expect to guarantee variable
   consistency by locking the mu fields in the Proxys exported from here. 
*)

(* the instances of T returned by this interface will not raise 
   SX.Uninitialized, as long as the init method in this interface has
   been called.
*)

(* note that this interface practices rep exposure!  The sx's actually
   used are returned by the routines, not copies thereof. *)

(* As things in this interface spawn threads and put themselves on the
   stacks of the spawned threads, they are not automatically garbage
   collected.  At some point, we have to add WeakRef code to make sure
   that they are, but that hasn't been done yet. *)

TYPE
  Super <: SuperClass;

  SuperClass <: PubSuper;

  PubSuper = VarUI.VarProxy OBJECT METHODS
    toText(elem : Elem.T) : TEXT;
    fromText(text : TEXT) : Elem.T RAISES { SXConversion.Error };
    getValue() : Elem.T  RAISES { SX.Uninitialized };          
    setValue(to : Elem.T);        
  END;
  
  T <: Public;

  Public = Super OBJECT METHODS
    init(val : Elem.T; mode := VarUI.ProxyMode.ReadWrite) : T;
    sx() : SXElem;
  END;

  SXElem = SXElemType.Var OBJECT p : T END;

  (* the purpose of using a SXElem is so that a client of this
     interface can maintain *either* an SXElem *or* a T and have the
     same information, without needing to remember both *)


  (**********************************************************************)

  (* A Watcher is a Proxy that watches an externally defined instance of
     an SX.T for changes, and presents that to a UI as a read-only 
     variable.  Useful for debugging and diagnostic interfaces. 

     The Watcher is relatively thread-efficient.  It spawns no threads
     unless registerCallback is called on the Watcher.  This also means
     it will be garbage collected without a doubt.

     It maybe really ought to be defined elsewhere, but we save a generic
     interface, a template, and instantiating a generic interface by putting
     it here.  Although we could of course put it in another generic interface
     that's instantiated by the same template file.
  *)

  Watcher <: PubWatcher;

  SXType = SXElemType.T;

  PubWatcher = Super OBJECT METHODS
    init(sx : SXElemType.T (* mode = VarUI.ProxyMode.ReadOnly *) ) : Watcher;
    sx() : SXElemType.T;
  END;

CONST Brand = "SXProxy(" & Elem.Brand & "," & SXElemType.Brand & ")";

END SXProxy.

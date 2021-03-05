(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Wed Aug 31 15:59:31 PDT 1994 by wobber *)
(*      modified on Sun Jan 12 16:17:07 PST 1992 by meehan *)

INTERFACE IP;

IMPORT Atom, AtomList;

(* This interface defines the addresses used for communicating
   with the internet protocol family.

   An IP ``endpoint'' identifies a running program in a way that allows
   other programs to communicate with it.

   An endpoint consists of an ``address'', which identifies the
   host machine on which the program is running, together with
   a ``port'', which distinguishes the program from other programs
   running on the same host.

   The host operating system guarantees that the same port is never
   in use by more than one program running on the host simultaneously.
   The same program may be identified by several ports.  Similarly, the
   internet police (try to) guarantee that the same address is never
   in use simultaneously by more than one machine in the world, but
   the same machine may be identified by several addresses.

   Port numbers and host addresses can be recycled: the operating system
   can reuse a port number of a program that has exited or explicitly
   freed the port, and the internet police will reassign addresses
   from old hosts to new ones. *)

TYPE EC = AtomList.T;

EXCEPTION Error(EC);

(* An IP implementation (or a layered IP protocolimplementation), can raise
   "Error" with error lists including, but not limited to, the following
   atoms: *)
(*TODO Dynamic linking to data does not work on Win32.*)
VAR
  LookupFailure, Unreachable, PortBusy, NoResources: Atom.T;

(* "LookupFailure" indicates that a call to "GetHostByName" could
   not determine whether the argument name exists.

   The following errors codes can arise from implementations of protocols
   which are layered on IP:

   "Unreachable" indicates that the destination protocol address is
   not reachable from the local node.  This is typically occurs
   in layered protocols (e.g. TCP) during connection establishment.

   "PortBusy" indicates that the caller attempted to use a port
   which is already in use.

   "NoResources" indicates an OS-dependent resource shortage (such
   as "no more sockets").  The remainder of the error list may detail
   the failure.
*)


TYPE
  Port = [0..65535];
  Address4 = RECORD a: ARRAY [0..3] OF BITS 8 FOR [0..255]; END;
  Address16 = RECORD a: ARRAY [0..15] OF BITS 8 FOR [0..255]; END;
  Address = Address4;
  Endpoint = RECORD addr: Address; port: Port END;

(* The type "Address" is an IP address in network byte order.
   The type "Port" is an IP port number in host byte order.
*)

  EP = OBJECT
    port : Port := NullPort;
  END;
  Endpoint4 = EP OBJECT
   adr : Address4 := NullAddress4;
  END;
  Endpoint16 = EP OBJECT
   adr : Address16 := NullAddress16;
  END;

CONST
  NullPort: Port = 0;
  NullAddress4 = Address{a := ARRAY OF BITS 8 FOR [0..255] {0,0,0,0}};
  NullAddress16 = Address16{a := ARRAY OF BITS 8 FOR [0..255] {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}};
  NullAddress = NullAddress4;
  NullEndPoint = Endpoint{NullAddress, NullPort};

PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN
     RAISES {Error};
(* If a host named "nm" is found, "GetHostByName" sets "res" to its
   address and returns "TRUE".  If "nm" is not found, "GetHostByName"
   returns "FALSE".  If the lookup cannot complete then "Error" is
   raised with "LookupFailure" in the error list. *)

(* For example,

| GetHostByName("gatekeeper.dec.com", addr)

  returns the address of the machine ``gatekeeper'' at DEC SRC.

  Different systems use different algorithms for
  implementing "GetHostByName". *)

PROCEDURE GetCanonicalByName(nm: TEXT): TEXT RAISES {Error};
(* If a host named "nm" is found, "GetCanonicalByName" returns the
   canonical, full-qualified name for the host "nm".  If "nm" is not found,
   "GetCanonicalByName" returns "NIL".  If the lookup cannot complete
   then "Error" is raised with "LookupFailure" in the error list. *)

(* For example,

| GetCanonicalName("gatekeeper.pa.dec.com")

  returns "gatekeeper.dec.com. *)

PROCEDURE GetCanonicalByAddr(addr: Address): TEXT RAISES {Error};
(* "GetCanonicalByAddr" is has the same semantics as "GetCanonicalByName"
   except that it takes an address rather than a name. *)

PROCEDURE GetHostAddr(): Address;
(* Return an address of the machine executing the call to "GetHostAddr". *)

PROCEDURE GetAddrInfo(READONLY name,service : TEXT) : EP RAISES {Error};
(* Return an address of the machine executing the call to "GetAddrInfo". *)

PROCEDURE GetNameInfo(ep : EP; VAR (* out *) host,service : TEXT) RAISES {Error};
(* Return the name for a machine with addr executing the call to "GetNameInfo" *)

END IP.

(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* A "Socket.T", or socket, is a file handle that provides access to
   a bidirectional channel that is typically used to communicate between
   processes.  *)

INTERFACE Socket;

IMPORT Atom, File, OSError, Thread;
FROM Cstdint IMPORT uint32_t;

TYPE
  T <: Public;
  Public = File.T OBJECT METHODS
    bind (READONLY ep: EndPoint)     RAISES {OSError.E};
    connect (READONLY ep: EndPoint)  RAISES {OSError.E, Thread.Alerted};
    accept (): T                     RAISES {OSError.E, Thread.Alerted};
    listen (max_queue: CARDINAL)     RAISES {OSError.E};
    bytes_available (): CARDINAL     RAISES {OSError.E};
    peek (): EndPoint                RAISES {OSError.E};
    this_end (): EndPoint            RAISES {OSError.E};
    other_end (): EndPoint           RAISES {OSError.E};

    send_to (READONLY ep: EndPoint;
             READONLY buf: ARRAY OF File.Byte)
      RAISES {OSError.E};

    recv_from (VAR(*OUT*) ep: EndPoint;
               VAR(*OUT*) buf: ARRAY OF File.Byte;
                          mayBlock := TRUE): INTEGER
      RAISES {OSError.E};
  END;

TYPE 
  Port     = [0..65535];
  AddressType = {IPv4, IPv6};
  AddressIPv4  = RECORD a: ARRAY [0..0] OF uint32_t; END;
  AddressIPv6  = RECORD a: ARRAY [0..3] OF uint32_t; END;
  Address = RECORD type: AddressType; ipv4: AddressIPv4; ipv6: AddressIPv6 END;
  EndPoint = RECORD addr: Address;  port: Port;  END;
  (* The type "Address" is an IP address in network byte order.
     The type "Port" is an IP port number in host byte order.
  *)

CONST 
  NullPort     : Port     = 0;
  NullAddressIPv4  = AddressIPv4 {a := ARRAY [0..0] OF uint32_t {0}};
  NullAddressIPv6  = AddressIPv6 {a := ARRAY [0..3] OF uint32_t {0,0,0,0}};
  NullAddress  = Address {AddressType.IPv4, NullAddressIPv4, NullAddressIPv6};
  NullEndPoint = EndPoint {NullAddress, NullPort};

VAR (*CONST*) FileType: File.Type;
(* Equal to {\tt Atom.FromText(\char'42Socket\char'42).} *)

PROCEDURE  Create (reliable: BOOLEAN): T  RAISES {OSError.E};
(* Create a new bidirectional channel.  If "reliable" is "TRUE", a
   TCP stream socket is created, otherwise an unreliable datagram
   socket is created. *)


(* A socket can raise "OSError.E" with error lists including,
   but not limited to, the following atoms: *)

VAR (*CONST*)
  Unreachable, PortBusy, NoResources: Atom.T;
  Refused, Timeout, ConnLost, Unexpected: Atom.T;

(* "Unreachable" indicates that the destination protocol address is
   not reachable from the local node.  This is typically occurs
   in layered protocols (e.g. TCP) during connection establishment.

   "PortBusy" indicates that the caller attempted to use a port
   which is already in use.

   "NoResources" indicates an OS-dependent resource shortage (such
   as "no more sockets").  The remainder of the error list may detail
   the failure.

   "Refused" indicates that the destination endpoint does not
   exist, or has been closed.

   "Timeout" indicates that a read or write failed due to
   a connection timeout.

   "ConnLost" indicates a broken TCP connection.

   "Unexpected" something else happened!
*)

END Socket.

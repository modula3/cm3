(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Thu Jun  3 10:04:16 PDT 1993 by wobber *)
(*      modified on Sun Jan 12 16:16:54 PST 1992 by meehan *)
(*      modified on Sat Jan 11 16:55:00 PST 1992 by gnelson *)

INTERFACE TCP;

(* A TCP connection is a bidirectional byte stream between two
   programs, implemented using internet protocols.

   Connections are symmetric, but the protocol for establishing a
   connection is asymmetric.  One of the programs (which we will call
   the ``server'') creates a ``connector'' associated with some IP port.
   Another program (which we will call the ``client'') creates a
   connection to the server by specifying the endpoint of the connector.
   The server should fork threads that block waiting to accept
   connections.
   
   In general the connector contains a queue of server threads waiting
   to accept connections and a queue of client threads waiting to 
   make connections.  In the idle state one of these queues is empty.
   Whenever both queues are non-empty, the client and server threads
   at the heads of their queues are unblocked and a connection is 
   created between them. *)

IMPORT Atom, ConnFD, IP, Thread;

TYPE
  Connector <: ROOT;
  T <: ConnFD.T;
     
(* The TCP implementation produces a "ConnFD.T" which raises
   "Rd.Failure" and "Wr.Failure" errors with the same atom lists
   used for "IP.Error".  TCP procedures can raise "IP.Error" with
   atom lists including those atoms described in "IP.i3", plus the
   following atoms: *)

VAR
  Refused, Closed, Timeout, ConnLost: Atom.T;

(* "Refused" indicates that the destination endpoint does not
   exist, or has been closed.

   "Closed" indicates an attempt to access a closed "T" or aa
   closed "Connector".

   "Timeout" indicates that a read or write failed due to
   a connection timeout.

   "ConnLost" indicates a broken TCP connection.
*)


(* procedures *)

PROCEDURE NewConnector(ep: IP.Endpoint): Connector RAISES {IP.Error};
(* Allocate and return a connector with the endpoint "ep". *)
   
(* The value of "ep.addr" must be null or one of the caller's IP
   addresses.  If "ep.addr" is null then the connector will accept
   connections on any of the caller's addresses.

   If "ep.port # IP.NullPort", then the connector offers connections on 
   "ep.port".  In this case the procedure raises "IP.Error" with
   "IP.PortBusy" if "ep.port" is not available.
   
   If "ep.port = IP.NullPort", then the operating system selects some
   available port and assigns it to the connector, which will then
   accept connections only on that port.  You can find out which port
   was assigned by using the "GetEndPoint" procedure below.  If no ports
   are available, the procedure raises "IP.Error" with "IP.NoResources".
   
   Initially the connector returned has no server or client threads
   waiting on it. *)

PROCEDURE GetEndPoint(c: Connector): IP.Endpoint;
(* Return the endpoint of "c". *)

PROCEDURE CloseConnector(c: Connector);
(* Close the connector "c".  Not yet implemented. *)

PROCEDURE Connect (ep: IP.Endpoint): T
    RAISES {IP.Error, Thread.Alerted};
(* Create and return a connection to the endpoint "ep". *)

(* The procedure "Connect" raises "IP.Error" with "Refused" if no connector
   exists with the given endpoint or if the connector with that endpoint
   is closed.  Otherwise the thread calling "Connect" is blocked on the
   queue of clients waiting to make connections on that connector. When
   it reaches the head of the queue and the queue of server  threads is
   non-empty, a connection is created between it and the server thread;
   the connection is returned by "Connect".

   "Connect" raises "IP.Error" with "TimeOut" or "IP.Unreachable"
   if the endpoint is crashed or if communication fails. It raises "IP.Error"
   with "Refused" if the connector is closed while the thread is blocked.

   If "ep.addr" is null, "Connect" connects to the calling machine
   itself. *)

PROCEDURE Accept(c: Connector): T
    RAISES {IP.Error, Thread.Alerted};
(* Block and wait until some process connects to "c", and
   return a connection to that process.  *)

(* "Accept" is the server-side analog of "Connect": the thread calling
   "Accept" is blocked on the queue of threads waiting to service
   connectins on "c".  When the thread reaches the head of the queue
   and the queue of client threads is non-empty, a connection is created
   between it and the client thread; the connection is returned by
   "Accept".v

   "Accept" raises "IP.Error" with an list including "Closed"
   if the connector is closed or becomes closed while the thread is
   blocked.  *)

PROCEDURE Close(t: T);
(* Close the connection "t" and free all related resources. *)


END TCP.

    

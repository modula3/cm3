(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* StubConn.i3 *)
(* Last modified on Mon Nov  7 12:34:49 PST 1994 by wobber  *)
(*      modified on Wed Dec  2 11:40:50 PST 1992 by gnelson *)
(*      modified on Wed Jun 24 11:12:25 PDT 1992 by owicki *)


(* A "StubLib.Conn" represents a bi-directional connection used to invoke
   remote methods by the network objects runtime.  Here we reveal that
   a connection "c" consists of a message reader "c.rd" and a message
   writer "c.wr".\ttindex{StubLib.Conn}

   Connections come in matching pairs; the two elements of the pair 
   are typically in different address spaces.  If "c1" and "c2"
   are paired, the target of "c1.wr" is equal to the source of "c2.rd", 
   and vice versa.  Thus the messages written to "c1.wr" can be read 
   from "c2.rd", and vice versa. *)
  
INTERFACE StubConn;

IMPORT MsgRd, MsgWr, StubLib;
   
REVEAL StubLib.Conn <: Public;  

TYPE 
  T = StubLib.Conn;      (* compatibility with old stub generator, remove *)
  Public = OBJECT rd: MsgRd.T; wr: MsgWr.T END;

END StubConn.

(* Clients can use this interface to bypass the procedures in the
   "StubLib" interface and marshal and unmarshal arguments using
   inline code, for example to write directly to the underlying
   writer.  To do this, import the "RdClass" and "WrClass"
   interfaces\cite{Modula3} to reveal the internal structure of
   readers and writers.  You will have to be careful about locks.  All
   readers and writers contain an internal lock used to serialize
   operations.  It is a requirement of the "StubLib" interface that
   all parameters of type "Conn" be passed with both streams unlocked.
   It is a further requirement that no client thread operate on the
   streams while an activation of a "StubLib" procedure is in
   progress.

   There are two final clauses in the specification of the message
   readers and message writers that appear in a "StubConn.T".  First,
   their buffers must be word-aligned in memory. More precisely, if
   byte "i" in the data stream is stored in the buffer at memory
   address "j", then "i" and "j" must be equal modulo the
   machine word size. This requirement allows optimized stubs to read
   and write scalar values from the buffer efficiently.  Second, their
   buffers must not be too small.  More precisely, when the
   "nextMsg" method of a writer returns, there must be at least 24
   bytes of free space in the writer buffer, and when the
   "nextMsg" method of a reader returns, there must be at least 24
   bytes of message data in the reader buffer. This requirement allows
   the runtime to efficiently read and write the headers required by
   the network object protocol.\index{buffered streams}  *)





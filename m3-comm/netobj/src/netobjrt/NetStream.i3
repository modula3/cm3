(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* NetStream.i3 *)
(* Last modified on Mon Nov  7 12:08:21 PST 1994 by wobber  *)


(* The "NetStream" interface describes the marshaling of readers and
   writers, and provides procedures that you will need to use if
   you plan to reuse a stream after marshaling it.

   The network object runtime allows subtypes of "Rd.T" and "Wr.T" to be
   marshaled as parameters and as results of remote method invocation.
   To communicate a reader or writer from one program to
   another, a surrogate stream is created in the receiving program.
   We call the original reader or writer the concrete stream.
   Data is copied over the network between the concrete stream and the
   surrogate stream.  Surrogate streams are free-standing entities,
   valid beyond the scope of the remote call that produced them.
   Data can be transmitted on a surrogate stream at close to the
   bandwidth supported by the underlying transport.  \index{buffered
   streams}\ttindex{Rd.T}\ttindex{Wr.T}

   The initial position of the surrogate reader or writer equals the
   position of the corresponding concrete stream at the time it was
   marshaled.  All surrogate readers and writers are unseekable.  Data
   is transferred between surrogates and concrete streams in
   background.  Therefore, undefined behaviour will result if you 1)
   perform local operations on the concrete stream while a surrogate
   for it exists, or 2) create two surrogates for the same stream by
   marshaling it twice.  There is a mechanism, described below, for
   shutting down a surrogate stream so that the underlying stream can
   be remarshaled.

   Calling "Wr.Flush" on a surrogate writer flushes all
   outstanding data to the concrete writer and flushes the concrete
   writer.  Calling "Wr.Close" flushes and then closes both the
   surrogate and the concrete writer.  Similarly, a call on
   "Rd.Close" on a surrogate closes both readers.

   Clients who marshal streams retain responsibility for closing them.
   For example, "Rd.Close" on a surrogate can fail due to the network,
   leaving the owner responsible for closing the concrete reader.
   The "WeakRef" interface can be used to register a GC cleanup
   procedure for this purpose.  

   The "ReleaseWr" procedure is used to shut down a surrogate
   writer so that the underlying writer can be reused.  It flushes
   any buffered data, closes the surrogate, and frees any network
   resources associated with the surrogate.  It leaves the concrete
   writer in a state where it can be reused locally or remarshaled.

   Similarly. the "ReleaseRd" procedure is used to shut down a
   surrogate reader so that the underlying reader can be reused.  It
   closes the surrogate, frees any network resources associated with
   the surrogate, and leaves the concrete reader in a state where it
   can be reused locally or remarshaled.  There is an important
   difference between releasing readers and writers: "ReleaseRd"
   discards any data buffered in the surrogate or in transit. *)

INTERFACE NetStream;

IMPORT Rd, Wr, Thread;

PROCEDURE ReleaseRd(rd: Rd.T)
    RAISES {Rd.Failure, Thread.Alerted};
(* If "rd" is a surrogate reader, release all network
   resources associated with "rd", discard all buffered data,
   close "rd", but do not close the concrete reader for "rd".
   This procedure is a no-op if "rd" is not a surrogate.
   \ttindex{NetStream.ReleaseRd} *)

PROCEDURE ReleaseWr(wr: Wr.T)
    RAISES {Wr.Failure, Thread.Alerted};
(* If "wr" is a surrogate writer, flush "wr", release
   all network resources associated with "wr", close "wr",
   but do not close the concrete writer for "wr".
   This procedure is a no-op if "wr" is not a surrogate.
   \ttindex{NetStream.ReleaseWr} *)

END NetStream.



(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jan 27 15:07:27 PST 1995 by msm      *)
(*      modified on Mon Oct 25 12:27:26 PDT 1993 by sfreeman *)

(* a remote client for a JVideo server.  These are immutable.  If you want
   different paramters, close this one and open a new one *)

INTERFACE JVSink;

IMPORT Atom, JVBuffer, jvprotocol, JVConverter, JVFromSource, OSError,
       Thread;

TYPE Quality = [0 .. NUMBER(jvprotocol.ClientQualityVal) - 1];
CONST DefaultQuality = 8;

(* range of acceptable values for image quality *)

(* error atoms which may be returned in an OSError.E *)
VAR
  hostNotFound: Atom.T;          (* may be raised by init or start *)
  sinkError   : Atom.T;          (* error comes from this module *)

TYPE
  T <: Public;
  Public = JVConverter.T OBJECT
           METHODS
             (* all methods are LL < self *)
             init (hostname: TEXT;   (* where is the server? *)
                   quality : Quality := DefaultQuality;
                   maxBuffers: CARDINAL := 2; (* max buffers allowed in the
                                                 associated buffer pool *)
                   factory: JVBuffer.Factory;
                   delay: CARDINAL := 0): T
                   RAISES {OSError.E, Thread.Alerted};
             (* initial setup.  establish a connection with the local and
                remote JV servers and create a buffer pool.  "factory" is
                used to create new buffers *)

             getInfo (VAR info: JVFromSource.StreamInfo): BOOLEAN;
             (* if the data in "info" is older than the info in the "T", as
                determined by the serial, set the values in info and return
                TRUE.  Otherwise, just return FALSE *)
           END;

END JVSink.

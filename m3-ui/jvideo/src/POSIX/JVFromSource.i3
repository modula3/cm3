(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Feb 13 03:29:14 PST 1995 by msm      *)
(*      modified on Tue Aug 17 23:30:21 PDT 1993 by sfreeman *)

(* subclass of JvsBuffer for holding frames from a JVSource *)

INTERFACE JVFromSource;

IMPORT Ctypes, JVBuffer, Jvs, JvsBuffer, jvprotocol;

TYPE
  StreamInfo = RECORD
                 serial : JVBuffer.Serial := 0; (* timestamp for info *)
                 kind   : Ctypes.int := jvprotocol.JVP_KIND_JPEG;
                 qfactor: Ctypes.int := 0;
                 width, height: CARDINAL := 0;
               END;

TYPE
  T_Public = JvsBuffer.T BRANDED OBJECT info: StreamInfo;  END;
  T <: T_Public;

TYPE
  Factory <: FactoryPublic;
  FactoryPublic = JvsBuffer.Factory OBJECT
                  METHODS
                    init (jvs: Jvs.T): Factory;
                    (* the "jvs" is used to create shared memory buffers.
                       It should already have been opened *)
                  END;

END JVFromSource.

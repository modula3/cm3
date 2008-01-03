(*
     Sqrt.ig
        A portable square root routine
        David Goldberg, Xerox PARC
        goldberg@parc.xerox.com
        November, 1993
*)

(* Copyright (c) 1993 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

(* Last modified on Sun Nov 14 20:07:56 PST 1993 by goldberg *)

GENERIC INTERFACE Sqrt(Type);

IMPORT FloatMode;

PROCEDURE Sqrt(x: Type.T): Type.T RAISES {FloatMode.Trap};

END Sqrt.

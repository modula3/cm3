(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Jul 20 17:09:06 PDT 1993 by sfreeman *)
(*      modified on Mon Oct 26 15:55:14 PST 1992 by msm *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE INTERFACE XProperties;

IMPORT X, VBT, TrestleComm, XClient, XClientF;

PROCEDURE InitialiseXClient (xcon: XClient.T) RAISES {TrestleComm.Failure};
(* setup the XClient so it will look out for XSelectionRequest events *)

PROCEDURE StartSelection (trsl      : XClient.T;
                          w         : X.Window;
                          type, prop: X.Atom;
                          sel       : VBT.Selection;
                          ts        : VBT.TimeStamp;
                          stackSize                   := 20000);

PROCEDURE ExtendSel (VAR sa: XClientF.SelArray; s: VBT.Selection);
PROCEDURE ExtendOwns (VAR sa: XClientF.OwnsArray; s: VBT.Selection);
(* replace the given array with a larger one and copy the contents of the
   old one into the new one *)

PROCEDURE AwaitConversion (v              : XClient.T;
                           w              : X.Window;
                           name, type, sym: X.Atom;
                           ts             : VBT.TimeStamp;
                           limit                            := 10): X.Atom
  RAISES {VBT.Error};

PROCEDURE ReadXSelFromProp (v: XClient.T; w: X.Window; prop, type: X.Atom):
  TEXT RAISES {VBT.Error};

TYPE
  PropertyWaitFor <: PWF_Public;
  PWF_Public = XClientF.SimpleWaitFor OBJECT
                 a    : X.Atom;
                 ts   : VBT.TimeStamp := 0;
                 state: INTEGER;
               END;

PROCEDURE UnlockedPutProp (         trsl      : XClient.T;
                                    w         : X.Window;
                                    prop, type: X.Atom;
                           READONLY data      : ARRAY OF CHAR;
                                    format    : INTEGER        )
  RAISES {TrestleComm.Failure};

PROCEDURE PutProp (         v         : XClient.T;
                            w         : X.Window;
                            prop, type: X.Atom;
                   READONLY data      : ARRAY OF CHAR;
                            format    : INTEGER        )
  RAISES {TrestleComm.Failure};

PROCEDURE UnlockedGetProp (             trsl  : XClient.T;
                                        w     : X.Window;
                                        prop  : X.Atom;
                           VAR (* OUT*) type  : X.Atom;
                           VAR (* OUT*) data  : REF ARRAY OF CHAR;
                           VAR (* OUT*) format: INTEGER            ):
  BOOLEAN RAISES {TrestleComm.Failure};

PROCEDURE GetProp (             v     : XClient.T;
                                w     : X.Window;
                                prop  : X.Atom;
                   VAR (* OUT*) type  : X.Atom;
                   VAR (* OUT*) res   : REF ARRAY OF CHAR;
                   VAR (* OUT*) format: INTEGER            ): BOOLEAN
  RAISES {TrestleComm.Failure};

END XProperties.


(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Mon Feb 15 20:03:17 PST 1993 by johnh *)
(* modified on Tue May 12 08:51:01 1992 by mhb *)

INTERFACE Classes;

(* Maintains lists of algorithm and view classes for ZeusPanel.  This
   module does its own internal locking to prevent simultaneous access.  
   LL of all procedures is arbitrary. *)

(* The "Sample" procedures return an internally-held copy of the requested
   algorithm or view.  ZeusPanel is allowed to call the methods of these
   copies only if they have no side effects. *)

IMPORT Algorithm, View, ZeusPanel;

EXCEPTION NotFound;


PROCEDURE RegisterAlg (proc: ZeusPanel.NewAlgProc; name: TEXT);

PROCEDURE FindAlg (name: TEXT): INTEGER RAISES {NotFound};

PROCEDURE NewAlg (which: INTEGER): Algorithm.T;

PROCEDURE AlgCount (): INTEGER;


PROCEDURE RegisterView (proc     : ZeusPanel.NewViewProc;
                        name     : TEXT;
                        alertable: BOOLEAN;
                        sample   : View.T                 );

PROCEDURE FindView (name: TEXT): INTEGER RAISES {NotFound};

PROCEDURE NewView (which: INTEGER): View.T;

PROCEDURE SampleView (which: INTEGER): View.T;

PROCEDURE ViewCount (): INTEGER;


END Classes.

(* Copyright 1990-1993 Digital Equipment Corporation.                     *)
(* Distributed only by permission.                                        *)
(*                                                                        *)
(* Postcard - UI for built-in display                                     *)
(*                                                                        *)
(* Last modified on Thu Apr 21 12:59:12 PDT 1994 by birrell               *)
(*      modified on Tue Feb 16 23:01:36 PST 1993 by meehan                *)

INTERFACE BuiltInDisplay;

IMPORT Closure, MultiSplit;

TYPE
  T <: Public;
    (* There is only one object of this type; it represents the state of the
       message display sub-windows, as a whole.  The "Display" method ensures
       there is a display sub-window, and tries to display the message there.
       There can be multiple display sub-windows, because they contain
       a "Detach" button internally; but only one of the display
       sub-windows is manipulable through this interface. *)

  Public = OBJECT
    METHODS
      Init(cl: Closure.T; splitter: MultiSplit.T): T;
        (* Initialize, and read the FV template, crashing if not available.
           The sub-windows will be Multi.children of "splitter" *)
      Display(folder, messageID: TEXT);
        (* Display specified message. *)
        (* LL < VBT.mu *)
    END;

END BuiltInDisplay.

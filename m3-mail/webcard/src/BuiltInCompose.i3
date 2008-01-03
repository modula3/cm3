(* Copyright 1990-1993 Digital Equipment Corporation.                     *)
(* Distributed only by permission.                                        *)
(*                                                                        *)
(* Postcard - UI for built-in composition                                 *)
(*                                                                        *)
(* Last modified on Thu Apr 21 14:44:51 PDT 1994 by birrell               *)
(*      modified on Tue Feb 16 23:01:35 PST 1993 by meehan                *)

INTERFACE BuiltInCompose;

IMPORT Closure, FormsVBT, MultiSplit, VBT;

TYPE
  T <: Public;
    (* There is only one object of this type; it represents the state of the
       message composition sub-windows, as a whole.  The "Compose" method
       ensures there is at least one composition sub-window, and tries places 
       the draft in the lowest numbered idle sub-window.  The composition 
       sub-windows provide editing, and a "Send" button. *)

  Public = OBJECT
      ownWindow: VBT.T; 
        (* separate window for drafts, or NIL. Read-only to client. *)
        (* LL = VBT.mu *)
    METHODS
      Init(cl: Closure.T): T;
        (* Initialize, and read the FV template, crashing if not available.
           The sub-windows aren't usable until after SetSplitter. *)
      Compose(to, cc, fcc, subject, inReplyTo, body: TEXT);
        (* Place the specified draft in a composition sub-window. *)
        (* LL < VBT.mu *)
      SetSplitter(splitter: MultiSplit.T; doCompose: FormsVBT.Proc;
                  near: VBT.T);
        (* If splitter # NIL, use this for children; otherwise create
           and use a separate "drafts" window; "doCompose" is suitable
           for a window's "compose" button. *)
        (* LL = VBT.mu *)
      SetExternal(external: BOOLEAN);
        (* LL = VBT.mu *)
        (* Record the clients preference about external editing *)
    END;

END BuiltInCompose.

(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Postcard - a user interface for mail and news *)
(* UI for accessing the "ni" program *)

(* Last modified on Thu Apr 21 16:23:50 PDT 1994 by birrell   *)

INTERFACE NI;

IMPORT Closure, FormsVBT, TextList, VBT;

EXCEPTION Error(TEXT);

TYPE
  T <: Public;

  Public = MUTEX OBJECT
  METHODS
    init(cl: Closure.T; fv: FormsVBT.T): T;
      (* LL = VBT.mu *)
      (* Initialize state for the browser dialog UI *)
    reset(time: VBT.TimeStamp);
      (* LL = VBT.mu *)
      (* Reset predicate to default state *)
    flip(time: VBT.TimeStamp) RAISES { Error };
      (* LL = PostcardMain.actions *)
      (* Flip to or from structured editor mode *)
    count(): CARDINAL RAISES { Error };
      (* LL = PostcardMain.actions *)
      (* Pose the query and return the count of messages that match *)
    browse() RAISES { Error };
      (* LL = PostcardMain.actions *)
      (* Pose the query and load the result into the message browser *)
    showConversation(msgList: TextList.T; folder: TEXT) RAISES { Error };
      (* LL = PostcardMain.actions *)
      (* Construct a query that returns messages in the same conversation
         as the given header of the given folder, and load the result
         into the message browser. *)
  END;

END NI.

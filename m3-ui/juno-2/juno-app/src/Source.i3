(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Oct 26 12:56:57 PST 1997 by heydon                   *)
(*      modified on Wed Aug  2 16:12:40 PST 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:51:53 PDT 1992 by myers                    *)

(* A "Source.T" represents the state of the source view. *)

INTERFACE Source;

IMPORT View, VBT, JunoAST, JunoCompileErr;

TYPE
  T <: Public;
  Public = View.T OBJECT METHODS
    init(root: View.Root): T;
  END;

(* "NEW(T).init(r)" creates a new source view on the root "r". *)

PROCEDURE Compile(s: T; time: VBT.TimeStamp; skipify: BOOLEAN): BOOLEAN;
(* Take the following two steps:

|  1. If the current command is out-of-date, parse the current command, and
|  2. Compile the current command (reflecting "skipify") and install its
      compiled code.

   If any parse or compilation errors occur, display an error message,
   highlight the error using timestamp "time". Otherwise, set "s.root.astTrue"
   to "TRUE". *)

PROCEDURE Make(s: T; time: VBT.TimeStamp; skipify: BOOLEAN): BOOLEAN;
(* Call "Compile(s, time, skipify)". If that succeeds, update the drawing
   (thereby setting "s.root.dTrue" to "TRUE") and unparse the current command
   (thereby setting "s.root.sTrue" to "TRUE"). The current command that gets
   unparsed will contain any new hints engendered by running the current
   command when the drawing is updated.

   Returns "TRUE" iff no lex, parse, compilation, or run-time errors
   occurred. *)

PROCEDURE ShowError(
    s: T; ast: JunoAST.T;
    READONLY err: JunoCompileErr.ErrVal;
    ts: VBT.TimeStamp);
(* Unparse "ast" to "s", highlight its subtree "err.ast", and display the
   message "err.msg" to the user. If "err.ast" is not a subtree of "ast", the
   error message and error tree are written to standard error. *)

PROCEDURE GetText(s:T): TEXT;
(* Return the contents of "s". *)

PROCEDURE SetText(s: T; txt: TEXT);
(* Set the contents of "s" to "txt".  Used when restoring a checkpoint. *)

END Source.

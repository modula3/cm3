(* --------------------------------------------------------------------- *)
(* 07-JUN-96  JK  Added ReplaceTags procedure.                           *)
(* --------------------------------------------------------------------- *)
(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Fri Jun 28 00:43:25 PDT 1996 by mhb      *)
(*      modified on Wed Jun 23 13:11:24 PDT 1993 by steveg   *)
(*      modified on Wed Aug 19 16:29:11 PDT 1992 by sclafani *)
<*PRAGMA LL*>

INTERFACE CodeView;

(* Dynamic display and highlighting of algorithm source code.

   An CodeView.T displays a visual trace of program execution.  When a
   procedure is called, a window pops up containing the source for the
   procedure, with the header highlighted.  As the procedure runs, each
   statement is highlighted as it is executed.  When a procedure exits, its
   window is deleted.

   The source code displayed is not necessarily the same as the code being
   executed.  An arbitrary file can be annotated and displayed as the
   source code.  The annotations indicate sections of the file to be used
   as the source code for a procedure, and delimit regions to be
   highlighted at a given point in the running program.

   Regions are numbered within a procedure.  A procedure header has the
   form "@procedure-name\n".  A matching trailer marks the end of the
   procedure.  Procedures must be disjoint.  The procedure header
   implicitly starts region #0.  Other regions are started with numbered
   tags of the form "@#### ", where '#' is a decimal digit.  A single '@'
   character delimits the end of a region.  When an '@' character appears
   in the source, another '@' can be used to quote it.  No annotation
   characters appear in the displayed source, including the trailing
   newline in procedure headers and the trailing space in region tags. *)

(*| Here is an example of an annotated procedure:

       @GCD
       int GCD (u, v)@
       int u, v;
       {
           @1 if (v = 0)@ @2 return u;@ else @3 return GCD (v, u % v);@
       }
       @GCD

   and this is the output from /Dump/ (see below):

       GCD
           0  int GCD (u, v)
           1  if (v = 0)
           2  return u;
           3  return GCD (v, u % v);

   This source code will drive the view:

       PROCEDURE GCD (u, v: INTEGER): INTEGER =
         VAR gcd: INTEGER;
         BEGIN
           view.enter ("GCD");
           view.at (1);
           IF v = 0 THEN
             view.at (2);
             gcd := u;
           ELSE
             view.at (3);
             gcd := GCD (v, u MOD v);
           END;
           view.exit ();
           RETURN gcd;
         END GCD;
*)

IMPORT RefList, Rd, VBT, Wr, ZSplit;

CONST
  DefaultFont = ARRAY OF
                  TEXT {"-*-courier-bold-r-*-*-*-120-*-*-*-*-iso8859-1"};

TYPE
  T <: Public;
  Public =
    ZSplit.T OBJECT
      pauseTime: CARDINAL := 400000;
      (* Default time, in microseconds, that methods should pause
         before returning.  If a negative /pauseTime/ value is
         passed to a method, this value is used. *)
    METHODS
      <* LL.sup = VBT.mu *>
      enter (procedureName: TEXT; pauseTime := -1);
      (* Indicates that procedure /procedureName/ has been
         entered.  Causes a window to pop up containing the
         source for the procedure with the header highlighted. *)

      exit (pauseTime := -1);
      (* Indicates that the current procedure is about to exit.
         Its source window will be deleted. *)

      at (highlight: CARDINAL; pauseTime := -1);
      (* Highlights the region numbered /highlight/, indicating
         the current position within the procedure. *)

      event (highlight           := 0;
             pauseTime           := -1;
             procedureName: TEXT := NIL );
      (* If /name/ # NIL, invokes /enter/.  If /highlight/ < 0,
         invokes /exit/, otherwise invokes /at/. *)

      exitAll ();
      (* Deletes all source windows, returning to the initial
         state. *)

      <* LL <= VBT.mu *>
      listNames (): RefList.T;
      (* Returns a list of procedure names in the source. *)

      listRegions (procedureName: TEXT): RefList.T;
      (* Returns an unordered list of region numbers in the
         procedure.  If no procedure named /procedureName/
         exists, NIL is returned. *)

      init (         source     : Rd.T;
                     errorWr    : Wr.T     := NIL;
            READONLY fontName              := DefaultFont;
                     paneOffset : CARDINAL := 20;
                     background : VBT.T    := NIL          ): T;
      (* Parses the annotated source code to be used by the view.
         Errors are written to /errorWr/, or to Stdio.stderr if
         NIL.  Source code will be displayed in the font
         /fontName/.  Source panes will be successively offset by
         /paneOffset/ pixels.  If /background/ is non-NIL, it
         will be used as the CodeView/ZSplit background,
         otherwise a TextureVBT will be created and used. *)
    END;

PROCEDURE New (         source     : Rd.T;
                        errorWr    : Wr.T     := NIL;
               READONLY fontName              := DefaultFont;
                        paneOffset : CARDINAL := 20;
                        background : VBT.T    := NIL ): T;
<* LL <= VBT.mu *>
(* "New (...)" is equivalent to "NEW (T).init (...)". *)

PROCEDURE Dump (source: Rd.T; wr: Wr.T; errorWr: Wr.T := NIL);
(* Writes a textual dump to /wr/ of the procedure names, region indicies,
   and region text from the annotated source in /source/.  Errors are
   written to /errorWr/, or to Stdio.stderr if NIL. *)

PROCEDURE DoReplaceTags( replace: BOOLEAN := TRUE );
(* Instructs the codeview parser to replace tags such as "@12" with
   spaces. The default behavior is to remove the tags. The call should
   be made at the beginning of a program, as it is not protected by a
   mutex and is global to all views. *)

END CodeView.

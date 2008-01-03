(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Oct  6 16:11:11 PDT 1993 by mhb      *)
(*      modified on Tue Feb  9 16:26:29 PST 1993 by johnh    *)
(*      modified on Wed Aug 19 17:27:10 PDT 1992 by sclafani *)
<* PRAGMA LL *>

(* A "ZeusCodeView.T" is a subclass of a "View.T" with one
   additional field "cv", which is a "CodeView.T".  It
   encapsulates "CodeView.T" into the Zeus framework.  See
   "CodeView.i3" for details on using code views. *)

INTERFACE ZeusCodeView;

IMPORT Algorithm, CodeView, Rd, Thread, VBT, View, Wr;

TYPE
  T <: Public;
  Public = View.T OBJECT cv: CodeView.T END;


PROCEDURE New (name   : TEXT;
               source : Rd.T;
               alg    : Algorithm.T;
               errorWr: Wr.T          := NIL;
               READONLY fontName := CodeView.DefaultFont;
               paneOffset: CARDINAL := 20;
               background: VBT.T    := NIL ): T;
<* LL = VBT.mu *>
(* Creates and returns an initialized "T" with the given name,
   using "CodeView.New" with the last two arguments to create the
   "cv" field.  The "alg" argument is the algorithm for which
   this is a code view. *)

PROCEDURE Enter (alg: Algorithm.T; procedureName: TEXT; pauseTime := -1)
  RAISES {Thread.Alerted};
<* LL = {} *>
(* Indicates that procedure "procedureName" has been entered.  Causes a
   window to pop up containing the source for the procedure with the header
   highlighted. *)

PROCEDURE Exit (alg: Algorithm.T; pauseTime := -1) 
  RAISES {Thread.Alerted};
<* LL = {} *>
(* Indicates that the current procedure is about to exit.  Its source
   window will be deleted.  *)

PROCEDURE At (alg: Algorithm.T; highlight := 0; pauseTime := -1)
  RAISES {Thread.Alerted};
<* LL = {} *>
(* Highlights the region numbered "highlight", indicating the current
   position within the procedure.  *)

PROCEDURE Event (alg          : Algorithm.T;
                 highlight                    := 0;
                 pauseTime                    := -1;
                 procedureName: TEXT          := NIL )
  RAISES {Thread.Alerted};
<* LL = {} *>
(* If "name # NIL", invokes "Enter".  If "highlight < 0", invokes "Exit",
   otherwise invokes "At".  *)


(* For Zeus internal use: *)

TYPE
  Arg = REF RECORD (* argument of a code view event *)
              highlight           := 0;
              pauseTime           := -1;
              procedureName: TEXT := NIL
            END;

END ZeusCodeView.

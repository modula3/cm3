(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Mar 23 18:16:04 PST 1996 by heydon                   *)
(*      modified on Tue Feb 21 15:04:27 PST 1995 by gnelson                  *)
(*      modified on Fri Aug  7 21:51:50 PDT 1992 by myers                    *)
<* PRAGMA LL *>

(* An "Editor.T" is a "TextPort.T" containing a Juno source module together
   with a cache of Juno parse trees.  If "te" is a Juno editor, then "src[te]"
   is the text contained in "te" and "trees[te]" is the cache of parse trees.
   An editor "te" is {\it valid} iff "trees[te]" is the sequence of parse
   trees of type "JunoAST.Block" resulting from successfully parsing
   "src[te]". *)

INTERFACE Editor;

IMPORT JunoAST, JunoScope;
IMPORT TextPort, VBT, Atom;
IMPORT Wr, Rd;

VAR (*CONST*)
  PointToolSym, TextToolSym, SetToolSym, ParamSym, TemplToolSym: Atom.T;

TYPE
  T <: Public;
  Public = TextPort.T OBJECT METHODS
    init(src: TEXT; readOnly := FALSE): T;
    txtModified(); <* LL.sup = VBT.mu *>
    getToolType(nm: Atom.T; VAR (*OUT*) type: Atom.T): BOOLEAN;
    getMenu(nm: Atom.T): VBT.T
  END;

(* "NEW(T, txtModified := P).init(src, readOnly)" returns a valid "T"
   containing initial source "src" such that "P" will be called whenever the
   text is modified. The editor is read-only iff "readOnly = TRUE".

   The "txtModified" method is called whenever the text of the editor has been
   modified since the last time the TextPort's modified bit was reset. The
   default "txtModified" method is a no-op. 

   The call "ed.getToolType(nm, type)" sets "type" to "PointToolSym",
   "TextToolSym", "TemplToolSym", or "SetToolSym" and returns "TRUE" if "nm"
   is contained in one of these user interface pragmas. Otherwise, it returns
   "FALSE".

   The call "ed.getMenu(nm)" returns a menu of parameters associated with the
   procedure named "nm" in the editor "ed". The menu contains a menu tool for
   each value associated with "nm" in a "Param" user-interface declaration. *)

TYPE
  Forest <: ForestPublic;
  ForestPublic = OBJECT
    tree: JunoAST.Block;
    next: Forest := NIL;
  END;

PROCEDURE Trees(te: T): Forest;
(* Return "trees[te]". *)

PROCEDURE Valid(te: T): BOOLEAN;
(* Return "TRUE" iff "te" is valid. After the user makes a modification to
   "src[te]", "te" is invalid until the next successful call to "Parse". *)

PROCEDURE Parse(te: T; time: VBT.TimeStamp): BOOLEAN;
(* If "src[te]" is a syntactically correct Juno program, set "trees[te]" to
   make "te" valid and return "TRUE".  Otherwise, pretty-print and display an
   error to the user, and return "FALSE". *)

PROCEDURE Unparse(te: T; errast: JunoAST.T := NIL;
  msg: TEXT := NIL; time: VBT.TimeStamp := 0);
<* LL.sup < te *>
(* Unparse "trees[te]" to "te". Requires "errast" and "msg" are either both
   "NIL" or both non-"NIL". If "time # 0" and both are non-"NIL", then
   additionally highlight the unparsed text of "errast" using timestamp "time"
   if it appears in one of the trees, and pop-up an error message box
   containing "msg". *)

PROCEDURE AddTree(te: T; ast: JunoAST.T);
(* Append "ast" to the end of the list of trees in "te", and append an
   unparsed version of "ast" to "te"'s source. If "ast" is a procedure
   declaration for a procedure whose name has "CurrCmd" as a prefix, then push
   "ast" onto the editor's current command stack; if it is a UI declaration,
   then it must have the correct number of arguments, and any procedure it
   names must not currently be named in any other UI declaration in "te". *)

PROCEDURE NextCmdNum(te: T): CARDINAL;
(* Return the number of the next available command on the current command
   stack. *)

PROCEDURE NextCmdName(te: T): Atom.T;
(* Return the name of the next available command on the current command
   stack. *)

PROCEDURE PopCurrCmd(te: T; VAR (*OUT*) nm: JunoAST.Id): JunoAST.Cmd;
<* LL.sup <= VBT.mu *>
(* Return the body of the procedure on the top of the current command stack,
   and set "nm" to the name of this procedure. If the body is of the form "IF
   VAR ... IN ... END FI", then the outer "IF ... FI" is stripped off the
   result. Returns NIL if the stack is empty or if the editor is not valid.
   In this case, the value of "nm" is undefined. *) 

PROCEDURE Width(tp: TextPort.T): CARDINAL;
(* Return the width in characters of "tp". *)

PROCEDURE ModuleName(te: T): Atom.T;
(* If the first non-comment block of "trees[te] is a "JunoAST.Module", return
   the module name; otherwise, return NIL. *)

PROCEDURE Compile(
    e: T;
    time: VBT.TimeStamp;
    scp: JunoScope.T;
    VAR (*OUT*) modName: JunoAST.Id;
    VAR (*OUT*) entity: JunoScope.Mod;
    uniqueModName := TRUE): BOOLEAN;
<* LL.sup < e *>
(* Parses (if necessary) and compiles the contents of the editor "e" under the
   scope "scp". If the compilation was successful, sets "modName" to the name
   of the module (or to "NIL" if there is no "MODULE" header), sets "entity"
   to a module entity whose "public" and "scp" scopes have parent scope "scp"
   and that contain bindings for public and all declarations, respectively,
   and returns "TRUE". Otherwise, displays an error to the user using
   event-time "time" and returns "FALSE".

   If "uniqueModName = TRUE", then if a module name is specified, it must not
   appear in "scp".

   This procedure does not pretty-print the contents of "e" or process
   the UI declarations in "e".  See "Unparse" and "EditorUI.CompileUI". 

   If successful, this procedure also has a side-effect on the Juno machine:
   it stores compiled versions of the predicates, functions and procedures
   appearing in "e" in the global code table. It also builds associations
   according to the "UI" pragmas in "e". *)

PROCEDURE SaveSlots(wr: Wr.T);
(* Write to "wr" the indexes of any internal "JunoRT" slots that are stored
   in the editor. *)

PROCEDURE RestoreSlots(rd: Rd.T);
(* Read from "wr" the indexs stored by "SaveSlots", and set the internal
   slots to the values read. *)

END Editor.

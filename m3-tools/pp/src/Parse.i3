(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri May 26 08:30:07 PDT 1995 by kalsow                 *)
(*      modified on Tue Jun 23 10:39:52 PDT 1992 by schilit@xerox.com      *)
(* Last modified on Mon Nov 25 17:42:20 PST 1991 by meehan                 *)
(*      modified on Thu Jul 25 20:18:11 PDT 1991 by stolfi                 *)
(*      modified on Wed Apr 24 11:39:22 1991 by nichols@xerox.com          *)
(*      modified on Sun Jun 10 05:45:18 1990 by muller                     *)

INTERFACE Parse;

IMPORT NewFormatter AS Formatter, Ctypes, FBE;

TYPE
  Style = {SRC, EMULLER};
  Options =
    RECORD
      lowerCase : BOOLEAN;		(* allow lower-case keywords *)
      bodyFont: TEXT;           (* most text *)
      keywordFont: TEXT;        (* M3 keywords *)
      builtinIDFont: TEXT;	(* built-in ids *)
      procNameFont: TEXT;       (* procedure decls *)
      commentFont: TEXT;        (* most comments *)
      fixedCommentFont: TEXT;	(* no-reformat comments *)
      fixedFont: TEXT;          (* text and char literals *)
      offset: REAL;         (* indentation level in chars *)
      commentColumn: REAL;  (* where same-line comments go in chars *)
      style: Style;             (* END-alignment style *)
      alignDecls: BOOLEAN;      (* align VAR, CONST, etc. decls *)
      follow : BOOLEAN;		(* format as if comments follow decls *)
      breakType : Formatter.BreakType; (* controls line breaks *)
      callSpace : BOOLEAN;	(* write foo (x, y) instead of f(x, y) *)
    END;


PROCEDURE Init (         inputFile      : TEXT;
                         output         : Formatter.T;
                READONLY options        : Options;
                         calledFromEmacs: BOOLEAN      ) RAISES {FBE.Failed};

TYPE
  FontInfo = RECORD
    bf, kf, bif, pf  : FBE.Font;
    cf, fcf, ff      : FBE.Font;
  END;

<* EXTERNAL *>
PROCEDURE initParser (
  infile           : Ctypes.char_star;
  output           : Formatter.T;
  emacs            : INTEGER;
  lowerCase        : INTEGER;
  READONLY fonts   : FontInfo;
  offset           : LONGREAL;
  commentColumn    : LONGREAL;
  style            : INTEGER;
  alignDecls       : INTEGER;
  breakType        : INTEGER; (* == ORD (Formatter.BreakType) *)
  follow           : INTEGER;
  callSpace        : INTEGER;

  charWidth        : PROCEDURE (t: Formatter.T;
                                font: FBE.Font;
                                c: CHAR): LONGREAL;

  flush            : PROCEDURE (t: Formatter.T) RAISES {FBE.Failed};

  setFont          : PROCEDURE (t: Formatter.T;
                                font: FBE.Font) RAISES {FBE.Failed};

  putChar          : PROCEDURE (t: Formatter.T;
                                c: CHAR) RAISES {FBE.Failed};

  break            : PROCEDURE (t: Formatter.T;
                                offset: LONGREAL;
                                type := Formatter.BreakType.OptimalBreak;
                                freshLine: INTEGER) RAISES {FBE.Failed};

  newLine          : PROCEDURE (t: Formatter.T;
                                offset: LONGREAL;
                                freshLine: INTEGER) RAISES {FBE.Failed};

  unitedBreak      : PROCEDURE (t: Formatter.T;
                                offset: LONGREAL;
                                freshLine: INTEGER) RAISES {FBE.Failed};

  group            : PROCEDURE (t: Formatter.T) RAISES {FBE.Failed};

  begin            : PROCEDURE (t: Formatter.T;
                                offset: LONGREAL;
                                width: LONGREAL) RAISES {FBE.Failed};

  align            : PROCEDURE (t:          Formatter.T;
                                columns:    CARDINAL;
                                tryOneLine: INTEGER;
                                tryAlign:   INTEGER) RAISES {FBE.Failed};

  noAlign          : PROCEDURE (t: Formatter.T) RAISES {FBE.Failed};

  col              : PROCEDURE (t: Formatter.T;
                                column: LONGREAL;
                                relative: INTEGER;
                                space: LONGREAL) RAISES {FBE.Failed};

  end              : PROCEDURE (t: Formatter.T) RAISES {FBE.Failed};
  );

<*EXTERNAL*>
PROCEDURE yyparse ();

END Parse.

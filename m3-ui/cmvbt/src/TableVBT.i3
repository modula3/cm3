
(* Copyright 1997 Critical Mass, Inc. All Rights Reserved. *)
(* See file COPYRIGHT for full description. *)

INTERFACE TableVBT;

(* "TableVBT" manages a table of VBTs, with proper
   headings, and highlighting, similar to a spread sheet. 
   Each table uses two grid splits, one for its headings,
   and another for the contents. *)

IMPORT Font, VBT, PaintOp, Pixmap;
IMPORT GridSplit;

TYPE
  T <: Public; 
  Public = Private OBJECT METHODS
    init(READONLY colnames: ARRAY OF TEXT;
         READONLY colsizes: ARRAY OF CARDINAL;
                  fnt: Font.T := Font.BuiltIn;
                  nrows: CARDINAL := 0;
                  rowheight : CARDINAL := 24;
                  colwidth : CARDINAL := 150;
                  op : PaintOp.T := PaintOp.Bg;
                  txt: Pixmap.T := Pixmap.Solid): T;
    contents(): GridSplit.T;

    heading(col: CARDINAL; READONLY cd: VBT.MouseRec);
    content(row: CARDINAL; READONLY cd: VBT.MouseRec);

    numrows(): CARDINAL;

    insert(row: CARDINAL := LAST(CARDINAL); 
           READONLY data: ARRAY OF VBT.T);

    delete(row: CARDINAL; 
           VAR      data: ARRAY OF VBT.T);

  END;
  Private <: VBT.T;

  (* A call to "v.init" initializes "v" with
     column names and sizes "colnames" and "colsizes",
     with "fnt" as the font and with "nrows".
     Row height and column width are set to 
     "rowheight", "colwidth". Finally, "op" and
     "txt" provide the usual painting operations.
     It is a checked runtime error if 
     "NUMBER(colsizes) # NUMBER(colnames)".
     

     "v.contents" returns the grid split 
     for the "v". "v.heading" and "v.content"
     are callbacks which are called when 
     the user clicks on a column in the heading
     or a column in the contents. 

     "v.numrows" returns the number of rows.

     "v.insert" inserts a row with the content
     "data" in the contents.

     "v.delete" deletes a row of the table. *)

END TableVBT.

(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Feb  1 09:45:17 PST 1995 by kalsow *)
(*      modified on Wed Jun 22 15:53:03 PDT 1994 by bharat *)
(*      modified on Tue Jan 11 09:13:24 PST 1994 by mhb    *)
<* PRAGMA LL *>

MODULE RW;
IMPORT FloatMode, Fmt, Thread, Lex, Rd, Wr,  NodeVBT, Text;

CONST
  debug = FALSE;

<* FATAL FloatMode.Trap, Rd.EndOfFile, Rd.Failure, Lex.Error, Thread.Alerted, 
         NodeVBT.InvalidObjectName, Wr.Failure *>


PROCEDURE wint(w : Wr.T; o : INTEGER) =
  BEGIN
   
    Wr.PutText(w, Fmt.Int(o) & " ");
  END wint; 

PROCEDURE wbool(w : Wr.T; o : BOOLEAN) =
  BEGIN
    IF o THEN wtext(w, "TRUE") 
    ELSE wtext(w, "FALSE")
    END;
  END wbool; 

PROCEDURE wcard(w : Wr.T; o : CARDINAL) =
  BEGIN
    Wr.PutText(w, Fmt.Int(o) & " ");
  END wcard; 

PROCEDURE wtext( w: Wr.T; o : TEXT) = 
  BEGIN
    Wr.PutText(w, Fmt.Int(Text.Length(o)) & " " & o & " ");
  END wtext;

PROCEDURE wtuple( w: Wr.T; o : NodeVBT.Tuple) = 
  BEGIN
    wtext(w, o.class);
    wtext(w, o.name);
  END wtuple;

(*--------------------------------------*)

PROCEDURE rint(r : Rd.T; VAR i : INTEGER) =
  BEGIN
    Lex.Skip(r);
    IF debug THEN
    NodeVBT.print("Reading an integer" &
      "Lookahead = " & Text.FromChar(Rd.GetChar(r))
      & "\n"); Rd.UnGetChar(r);
    END;
    i := Lex.Int(r)
  END rint; 

PROCEDURE rbool(r : Rd.T; VAR i : BOOLEAN) =
VAR
  temp : TEXT;
  BEGIN
    Lex.Skip(r);
    IF debug THEN
    NodeVBT.print("Reading a boolean" &
      "Lookahead = " & Text.FromChar(Rd.GetChar(r))
      & "\n"); Rd.UnGetChar(r);
    END;
    rtext(r, temp);
    i := Text.Equal(temp, "TRUE");
  END rbool; 

PROCEDURE rcard(r : Rd.T; VAR i : CARDINAL) =
  BEGIN
    Lex.Skip(r);
    IF debug THEN
    NodeVBT.print("Reading a cardinal" &
      "Lookahead = " & Text.FromChar(Rd.GetChar(r))
      & "\n"); Rd.UnGetChar(r);
    END;
    i := Lex.Int(r)
  END rcard; 

PROCEDURE rtext( r: Rd.T; VAR i : TEXT) = 
  BEGIN
    Lex.Skip(r);
    IF debug THEN
    NodeVBT.print("Reading text" &
      "Lookahead = " & Text.FromChar(Rd.GetChar(r))
      & "\n"); Rd.UnGetChar(r);
    END;
    WITH len = Lex.Int(r) DO
      EVAL Rd.GetChar(r); (* skip blank *)
      i := Rd.GetText(r, len)
    END
  END rtext;

PROCEDURE rtuple( r : Rd.T; VAR i: NodeVBT.Tuple) = 
  BEGIN
    Lex.Skip(r);
    IF debug THEN
    NodeVBT.print("Reading a tuple" &
      "Lookahead = " & Text.FromChar(Rd.GetChar(r))
      & "\n"); Rd.UnGetChar(r);
    END;
    rtext(r, i.class);
    rtext(r, i.name);
  END rtuple;

PROCEDURE ptot(i : NodeVBT.T) : NodeVBT.Tuple =
  BEGIN
    IF i = NIL THEN RETURN NodeVBT.Tuple{ "NIL", ""} 
    ELSE
      RETURN NodeVBT.Tuple{ NodeVBT.GetNodeTypeName(i), 
                            i.name }
    END
  END ptot;

PROCEDURE ttop( i : NodeVBT.Tuple) : NodeVBT.T =
  BEGIN
    IF Text.Equal(i.class, "NIL") THEN RETURN NIL
    ELSE
      RETURN NodeVBT.GetNodeNamed (i.name, NodeVBT.NameToIndex(i.class));
      (* this rigmarole is because the class index may have changed but not
         the name - because new classes were added, and the instance
         index may also differ since  the instanceList has been compacted *)
        
    END;
  END ttop; 

BEGIN
END RW.












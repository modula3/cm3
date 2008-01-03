(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr 12 10:35:02 PDT 1994 by kalsow                   *)

MODULE FilePath;

IMPORT ASCII, Pathname, Text, TextSeq;
FROM Msg IMPORT D;
(*
IMPORT M3Config, Fmt;
*)

PROCEDURE Normalize (dest, src: TEXT): TEXT =
  <* FATAL Pathname.Invalid *>
  VAR 
    i := 0; j := 0; sd := 0; ss := 0;
    destArcs, srcArcs, resArcs: Pathname.Arcs;
  BEGIN
    D("Normalize ", dest, ", ", src);

    IF src = NIL OR Text.Length(src) = 0 THEN
      RETURN dest;
    END;

    destArcs := Pathname.Decompose(dest);
    destArcs := TextSeq.Sub(destArcs, 1);
    sd := destArcs.size();
    (* D("sd=", Fmt.Int(sd), " "); *)

    srcArcs := Pathname.Decompose(src);
    srcArcs := TextSeq.Sub(srcArcs, 1);
    ss := srcArcs.size();
    (* D("ss=", Fmt.Int(ss), " "); *)

    i := 0;
    WHILE (i < MIN(sd - 1, ss - 1))
          AND (Text.Equal(srcArcs.get(i), destArcs.get(i))) DO
      INC(i);
    END;

    (* common prefix srcArcs[0 .. i-1], destArcs[0 .. i-1] *)

    (* D("i=", Fmt.Int(i), " "); *)
    IF (i = MIN(sd - 1, ss - 1)) AND (sd = ss) THEN
      (* src -> ./dest *)
      (* M("C1: ", Pathname.Last(dest)); *)
      RETURN Pathname.Last(dest);
    END;

    (* src -> .../destArcs[i .. sd] *)
    (* M("C2"); *)
    j := i;
    resArcs := NEW(Pathname.Arcs).init();
    resArcs.addlo(NIL); (* this is a relative path *)
    WHILE j < ss - 1 DO
      resArcs.addhi("..");
      INC(j);
    END;

    resArcs := TextSeq.Cat(resArcs, TextSeq.Sub(destArcs, i));

    WITH res = Pathname.Compose(resArcs) DO
      D(" --> ", res);
      RETURN res;
    END;
  END Normalize;

PROCEDURE CIEqual(t, u: TEXT): BOOLEAN RAISES {} =
  VAR
    lt: CARDINAL := Text.Length(t);
    lu: CARDINAL := Text.Length(u);
    i: CARDINAL := 0;
  BEGIN
    IF lt = lu THEN 
      WHILE i<lt DO
        IF ASCII.Upper[Text.GetChar (t, i)] # 
          ASCII.Upper[Text.GetChar (u, i)] THEN 
          RETURN FALSE 
        ELSE INC(i) 
        END;
      END;
      RETURN TRUE;
    ELSE RETURN FALSE
    END;
  END CIEqual;

PROCEDURE Classify (path: TEXT): Kind =
  VAR 
    ext := Pathname.LastExt(path);
  BEGIN
    IF ext = NIL THEN 
      RETURN Kind.other;
    END;
    IF CIEqual(ext, "m3") THEN
      RETURN Kind.M3;
    ELSIF CIEqual(ext, "i3") THEN
      RETURN Kind.I3;
    ELSIF CIEqual(ext, "ig") THEN
      RETURN Kind.IG;
    ELSIF CIEqual(ext, "mg") THEN
      RETURN Kind.MG;
    ELSIF CIEqual(ext, "tmpl") THEN
      RETURN Kind.TMPL;
    ELSIF CIEqual(ext, "quake") THEN
      RETURN Kind.QUAKE;
    ELSIF CIEqual(ext, "fv") THEN
      RETURN Kind.FV;
    ELSIF CIEqual(ext, "h") THEN
      RETURN Kind.H;
    ELSIF CIEqual(ext, "c") THEN
      RETURN Kind.C;
    END;
    RETURN Kind.other;
  END Classify;

BEGIN
END FilePath.



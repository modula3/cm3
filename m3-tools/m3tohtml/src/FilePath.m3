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

(* broken

PROCEDURE Normalize (dest, src: TEXT): TEXT =  (* FIXME !!!! *)
  VAR 
    n := 0;  d := dest;  s := src; up, res : TEXT;
    darcs, uarcs: Pathname.Arcs;
  BEGIN
    D("Normalize ", dest, ", ", src);
    IF s = NIL OR Text.Length(s) = 0 THEN
      RETURN d;
    END;
    darcs := Pathname.Decompose(dest);
    darcs := TextSeq.Sub(darcs, 1);
    s := Pathname.Prefix (s);
    d := Pathname.Prefix (d);
    (* s and d hold only the directory prefixes *)
    IF Text.Equal(s, d) THEN
      (* just the base was different *)
      RETURN Pathname.Last(dest);
    END;
    up := "";
    WHILE (s # NIL) AND (d # NIL) AND (Text.Length(s) > 0)
      AND NOT Text.Equal (s, d) DO
      s := Pathname.Prefix (s);
      d := Pathname.Prefix (d);
      IF Text.Empty(s) THEN
        up := Pathname.Parent;
      ELSE
        up := Pathname.Join(up, Pathname.Parent, NIL);
      END;
      INC (n);
    END;
    IF up # NIL THEN
      uarcs := Pathname.Decompose(up);
      uarcs := TextSeq.Sub(uarcs, 1);
    END;
    IF darcs = NIL THEN
      res := Pathname.Join(up, dest, NIL);
    ELSE
      darcs := TextSeq.Sub(darcs, uarcs.size());
      darcs.addlo(NIL);
      res := Pathname.Join(up, Pathname.Compose(darcs), NIL);
    END;
        (* s & Text.Sub (dest, Text.Length (d)); *)
    D(" --> ", res);
    RETURN res;
  END Normalize;
*)

(********************8
  optimized but partially broken original version

CONST
  Up = ARRAY [0..9] OF TEXT {
    "",
    "../",
    "../../",
    "../../../",
    "../../../../",
    "../../../../../",
    "../../../../../../",
    "../../../../../../../",
    "../../../../../../../../",
    "../../../../../../../../../"
  };

PROCEDURE NormalizePath (dest, src: TEXT): TEXT =
  VAR
    d_len   := Text.Length (dest);
    s_len   := Text.Length (src);
    m_len   := MIN (d_len, s_len);
    diff    := -1;
    n_extra := 0;
    result  := "";
    i       := 0;
    cd, cs  :  CHAR;
  BEGIN
    (* find the first arc where they differ *)
    IF (i < m_len) THEN
      cd := Text.GetChar(dest, i);
      cs := Text.GetChar(src, i);
    END;
    WHILE (i < m_len) AND (cd = cs) DO
      IF cd = '/' THEN diff := i; END;
      INC (i);
      IF (i < m_len) THEN
        cd := Text.GetChar(dest, i);
        cs := Text.GetChar(src, i);
      END;
    END;

    (* count the number of extra arcs in the source *)
    WHILE (i < s_len) DO
      IF cs = '/' THEN INC (n_extra) END;
      INC (i);
      IF (i < s_len) THEN
        cs := Text.GetChar(src, i);
      END;
    END;

    (* build the result *)
    WHILE (n_extra > LAST (Up)) DO
      result := result & Up [LAST (Up)];
      DEC (n_extra, LAST (Up));
    END;
    result := result & Up [n_extra] & Text.Sub (dest, diff+1);

    RETURN result;
  END NormalizePath;
*********************)

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
    END;
    RETURN Kind.other;
  END Classify;

BEGIN
END FilePath.



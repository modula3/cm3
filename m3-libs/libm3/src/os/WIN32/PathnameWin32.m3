(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Tue Jun 29 10:10:31 PDT 1993 by mcjones    *)
(*      modified on Wed May 12 16:56:05 PDT 1993 by meehan     *)
(*      modified on Mon May 10 20:58:46 PDT 1993 by mjordan    *)

MODULE PathnameWin32 EXPORTS Pathname;

IMPORT Text;

CONST
  DirSepChar = '\\'; DirSepText = "\\"; 
  DriveSepChar = ':';
  ExtSepChar = '.';  ExtSepText = ".";

CONST Legal = SET OF CHAR {'\001' .. '\177'} - SET OF CHAR {DirSepChar, ':'};
  (* *** This should be as permissive as any NT file system. *)

TYPE Visit = PROCEDURE(start, len: INTEGER);

PROCEDURE Valid(pn: T): BOOLEAN =
  PROCEDURE Ignore(<* UNUSED *> start, len: INTEGER) = BEGIN END Ignore;
  BEGIN
    TRY ParsePN(pn, visit := Ignore)
    EXCEPT Invalid => RETURN FALSE
    END;
    RETURN TRUE
  END Valid;

PROCEDURE Decompose(pn: T): Arcs RAISES {Invalid} =
  VAR arcs := NEW(Arcs).init();
  PROCEDURE Add(start, len: INTEGER) =
    BEGIN
      IF start >= 0 THEN arcs.addhi(Text.Sub(pn, start, len))
      ELSE arcs.addhi(NIL)
      END
    END Add;
  BEGIN
    ParsePN(pn, visit := Add);
    RETURN arcs
  END Decompose;

PROCEDURE Compose(a: Arcs): T RAISES {Invalid} =
  VAR t: TEXT;
  BEGIN
    WITH n = a.size() DO
      IF n = 0 THEN RAISE Invalid END;
      t := a.getlo();
      IF t # NIL THEN
        WITH nRoot = Text.Length(t) DO
          IF ParseRoot(t) # nRoot THEN RAISE Invalid END;
          IF Text.GetChar(t, 0) = DirSepChar AND
             Text.GetChar(t, nRoot - 1) # DirSepChar THEN
            t := t & DirSepText
          END
        END
      ELSE t := ""
      END;
      FOR i := 1 TO n-1 DO
        WITH arc = a.get(i) DO
          IF arc = NIL THEN RAISE Invalid END;
          FOR i := 0 TO Text.Length(arc) - 1 DO
            IF NOT Text.GetChar(arc, i) IN Legal THEN RAISE Invalid END
          END;
          t := t & arc
        END;
        IF i # n-1 THEN t := t & DirSepText END
      END;
      RETURN t
    END
  END Compose;

PROCEDURE Absolute(pn: T): BOOLEAN =
  BEGIN
    TRY RETURN ParseRoot(pn) # 0
    EXCEPT Invalid => RETURN TRUE (* started with \ *)
    END
  END Absolute;

PROCEDURE Prefix(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF baseLwb = 0 THEN RETURN "" END;
    TRY
      WITH nRoot = ParseRoot(pn) DO
        IF baseLwb <= nRoot THEN baseLwb := nRoot + 1 END
      END
    EXCEPT Invalid => RETURN pn
    END;
    RETURN Text.Sub(pn, 0, baseLwb - 1)
  END Prefix;

PROCEDURE Last(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN 
    NameSections(pn, baseLwb, baseUpb, extUpb);
    TRY
      WITH nRoot = ParseRoot(pn) DO
        IF baseLwb <= nRoot THEN baseLwb := nRoot END
      END
    EXCEPT Invalid => RETURN ""
    END;
    RETURN Text.Sub(pn, baseLwb, extUpb - baseLwb)
  END Last;

PROCEDURE Base(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN
    NameSections(pn, baseLwb, baseUpb, extUpb);
    RETURN Text.Sub(pn, 0, baseUpb)
  END Base;

EXCEPTION CheckedRuntimeError; <* FATAL CheckedRuntimeError *>

PROCEDURE Join(pn, base: T; ext: TEXT): T =
  VAR t := base;
  BEGIN 
    IF pn # NIL THEN
      IF Absolute(base) THEN RAISE CheckedRuntimeError END;
      WITH n = Text.Length(pn) DO
        IF n # 0 THEN
          WITH c = Text.GetChar(pn, n - 1) DO
            IF c # DirSepChar AND c # DriveSepChar THEN
              pn := pn & DirSepText
            END
          END;
          t := pn & t
        END
      END
    END;
    IF ext # NIL THEN t := t & ExtSepText & ext END;
    RETURN t
  END Join;

PROCEDURE LastBase(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN 
    NameSections(pn, baseLwb, baseUpb, extUpb);
    RETURN Text.Sub(pn, baseLwb, baseUpb - baseLwb)
  END LastBase;

PROCEDURE LastExt(pn: T): TEXT =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF baseUpb = extUpb THEN RETURN "" END;
    RETURN Text.Sub(pn, baseUpb + 1, extUpb - (baseUpb + 1))
  END LastExt;

PROCEDURE ReplaceExt(pn: T; ext: TEXT): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN 
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF baseUpb = extUpb THEN RETURN pn END; (* nothing to replace *)
    RETURN Text.Sub(pn, 0, baseUpb) & ExtSepText & ext
  END ReplaceExt;

(* Internal procedures. *)

PROCEDURE ParseRoot(t: TEXT): CARDINAL RAISES {Invalid} =
  (* Return the length of the longest prefix of "t" that has the form:
       \
       \\server\share
       drive:\
       drive:
     or raise "Pathname.Invalid" if "t" begins with something malformed. *)
  BEGIN
    WITH n = Text.Length(t) DO
      IF n = 0 THEN RETURN 0 END;
      WITH c = Text.GetChar(t, 0) DO
        IF c = DirSepChar THEN
          IF n = 1 OR Text.GetChar(t, 1) # DirSepChar THEN RETURN 1 END;
          VAR seenName, seenShare := FALSE; BEGIN
            FOR i := 2 TO n - 1 DO
              WITH cc = Text.GetChar(t, i) DO
                IF cc = DirSepChar THEN
                  IF seenShare THEN
                    IF seenName THEN RETURN i + 1 END;
                    RAISE Invalid
                  END;
                  seenShare := TRUE; seenName := FALSE
                ELSE
                  IF NOT cc IN Legal THEN RAISE Invalid END;
                  seenName := TRUE
                END
              END
            END;
            IF NOT seenShare OR NOT seenName THEN RAISE Invalid END;
          END;
          RETURN n
        END;
        IF ('a' <= c AND c <= 'z' OR 'A' <= c AND c <= 'Z') AND
           n > 1 AND Text.GetChar(t, 1) = DriveSepChar THEN
          IF n > 2 AND Text.GetChar(t, 2) = DirSepChar THEN RETURN 3 END;
          RETURN 2
        END;
        RETURN 0          
      END
    END
  END ParseRoot;

PROCEDURE ParsePN(pn: T; visit: Visit) RAISES {Invalid} =
  (* Call "visit(s, n)" for each arc name (including root) within "pn"
     starting at character "s" of length "n" characters. Special case:
     call "visit(-1, 0)" to indicate NIL root of relative pathname. *)
  VAR
    nRoot: CARDINAL; (* length of root, or zero if "pn" is relative *)
    s, e: CARDINAL; (* bounds of next arc to visit *)
  BEGIN
    WITH n = Text.Length(pn) DO
      nRoot := ParseRoot(pn);
      IF nRoot # 0 THEN visit(0, nRoot) ELSE visit(-1, 0) END;
      s := nRoot; e := s;
      WHILE e < n DO
        WITH c = Text.GetChar(pn, e) DO
          IF c = DirSepChar THEN
            visit(s, e - s);
            s := e + 1
          ELSIF NOT c IN Legal THEN
            RAISE Invalid
          END
        END;
        INC(e)
      END;
      IF nRoot # 0 AND nRoot = n THEN
        (* Map DirSepText to [DirSepText] rather than [DirSepText, ""]. *)
        (*SKIP*)
      ELSE visit(s, e - s) END
    END
  END ParsePN;

PROCEDURE NameSections(
    pn: T;
    VAR baseLwb, baseUpb, extUpb: CARDINAL)
  RAISES {} =
(* Perform the following assignments:
  baseLwb := the index of the first character of the last component of pn;
  baseUpb := the index of the character separating the extension and base
             of the last component of pn, or Length(pn) if there is no
             extension;
  extUpb  := Length(pn) *)
  VAR pos: CARDINAL; ch: CHAR;
  BEGIN
    extUpb := Text.Length(pn);
    pos := extUpb;
    baseUpb := extUpb;
    LOOP
      IF pos > 0 THEN
        DEC(pos);
        ch := Text.GetChar(pn, pos);
        IF ch = DirSepChar OR ch = DriveSepChar THEN
          baseLwb := pos + 1;
          EXIT
        ELSIF ch = ExtSepChar THEN
          baseUpb := pos
        END
      ELSE
        baseLwb := 0;
        EXIT
      END
    END
  END NameSections;

BEGIN
  Parent := "..";
  Current := "."
END PathnameWin32.

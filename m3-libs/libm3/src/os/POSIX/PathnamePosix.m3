(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Oct 25 10:31:06 PDT 1993 by mcjones    *)
(*      modified on Wed May 12 16:56:05 PDT 1993 by meehan     *)
(*      modified on Mon May 10 20:58:46 PDT 1993 by mjordan    *)

MODULE PathnamePosix EXPORTS Pathname;

IMPORT Text;

CONST
  DirSepChar = '/'; ExtSepChar = '.';
  DirSepText = "/"; ExtSepText = ".";

CONST Legal = SET OF CHAR {'\001' .. '\177'} - SET OF CHAR {DirSepChar};

PROCEDURE Valid(pn: T): BOOLEAN =
  PROCEDURE Ignore(<* UNUSED *> start, end: CARDINAL) = BEGIN END Ignore;
  BEGIN
    TRY EVAL ParsePosixPathname(pn, visit := Ignore)
    EXCEPT Invalid => RETURN FALSE
    END;
    RETURN TRUE
  END Valid;

TYPE Visit = PROCEDURE(start, end: CARDINAL);

PROCEDURE Decompose(pn: T): Arcs RAISES {Invalid} =
  VAR
    arcs := NEW(Arcs).init();
    absolute := ParsePosixPathname(pn, visit := Add);
  PROCEDURE Add(start, end: CARDINAL) =
    BEGIN arcs.addhi(Text.Sub(pn, start, end - start)) END Add;
  BEGIN
    IF absolute THEN arcs.addlo("/")
    ELSE arcs.addlo(NIL)
    END;
    RETURN arcs
  END Decompose;

PROCEDURE Compose(a: Arcs): T RAISES {Invalid}=
  VAR n := a.size(); t: TEXT;
  BEGIN 
    IF n = 0 THEN RAISE Invalid END;
    t := a.getlo();
    IF t # NIL THEN IF NOT Text.Equal(t, "/") THEN RAISE Invalid END
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
  END Compose;

PROCEDURE Absolute(pn: T): BOOLEAN =
  BEGIN
    RETURN Text.Length(pn) > 0 AND Text.GetChar(pn, 0) = DirSepChar
  END Absolute;

PROCEDURE Prefix(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF baseLwb = 0 THEN RETURN "" END;
    IF baseLwb = 1 THEN RETURN "/" END;
    RETURN Text.Sub(pn, 0, baseLwb - 1)
  END Prefix;

PROCEDURE Last(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN 
    NameSections(pn, baseLwb, baseUpb, extUpb);
    RETURN Text.Sub(pn, baseLwb, extUpb - baseLwb)
  END Last;

PROCEDURE Base(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF baseUpb + 1 = extUpb THEN RETURN pn END;
    RETURN Text.Sub(pn, 0, baseUpb)
  END Base;

EXCEPTION CheckedRuntimeError; <* FATAL CheckedRuntimeError *>

PROCEDURE Join(pn, base: T; ext: TEXT): T =
  VAR t: TEXT := base; l: CARDINAL;
  BEGIN 
    IF pn # NIL THEN
      IF Absolute(base) THEN RAISE CheckedRuntimeError END;
      l := Text.Length(pn);
      IF l # 0 THEN
        IF Text.GetChar(pn, l - 1) # DirSepChar THEN
          pn := pn & DirSepText
        END;
        t := pn & t
      END
    END;
    IF ext # NIL THEN t := t & ExtSepText & ext END;
    RETURN t
  END Join;

PROCEDURE LastBase(pn: T): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN 
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF baseUpb + 1 = extUpb THEN
      RETURN Text.Sub(pn, baseLwb, extUpb - baseLwb)
    END;    
    RETURN Text.Sub(pn, baseLwb, baseUpb - baseLwb)
  END LastBase;

PROCEDURE LastExt(pn: T): TEXT =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF extUpb # baseUpb THEN
      RETURN Text.Sub(pn, baseUpb + 1, extUpb - (baseUpb + 1))
    ELSE
      RETURN ""
    END
  END LastExt;

PROCEDURE ReplaceExt(pn: T; ext: TEXT): T =
  VAR baseLwb, baseUpb, extUpb: CARDINAL;
  BEGIN 
    NameSections(pn, baseLwb, baseUpb, extUpb);
    IF baseUpb = extUpb THEN RETURN pn END; (* nothing to replace *)
    RETURN Text.Sub(pn, 0, baseUpb) & ExtSepText & ext
  END ReplaceExt;


(* Internal procedures. *)

PROCEDURE ParsePosixPathname(pn: T; visit: Visit): BOOLEAN RAISES {Invalid} =
  (* Call "visit(s, e)" for each arc name starting at character "e" within
     "pn" of length "e-s" characters; return "TRUE" iff "pn" is absolute. *)
  VAR
    absolute: BOOLEAN;
    s, e: CARDINAL; (* bounds of next arc to visit *)
  BEGIN
    WITH n = Text.Length(pn) DO
      IF n # 0 AND Text.GetChar(pn, 0) = DirSepChar THEN
        absolute := TRUE; s := 1
      ELSE
        absolute := FALSE; s := 0
      END;
      e := s;
      WHILE e < n DO
        WITH c = Text.GetChar(pn, e) DO
          IF c = DirSepChar THEN
            visit(s, e);
            s := e + 1
          ELSIF NOT c IN Legal THEN
            RAISE Invalid
          END
        END;
        INC(e)
      END;
      (* We map "/" to ["/"] rather than ["/", ""]. *)
      IF absolute AND n = 1 THEN (*SKIP*) ELSE visit(s, e) END;
      RETURN absolute
    END
  END ParsePosixPathname;

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
  VAR pos: CARDINAL; ch: CHAR; extSepSeen := FALSE;
  BEGIN
    extUpb := Text.Length(pn);
    pos := extUpb;
    baseUpb := extUpb;
    LOOP
      IF pos > 0 THEN
        DEC(pos);
        ch := Text.GetChar(pn, pos);
        IF ch = DirSepChar THEN
          baseLwb := pos + 1;
          EXIT
        ELSIF ch = ExtSepChar THEN
          IF NOT extSepSeen THEN
            baseUpb := pos; extSepSeen := TRUE
          END
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
END PathnamePosix.

(* $Id$ *)

UNSAFE MODULE UnixUtils;
IMPORT Pathname;
IMPORT M3toC;
IMPORT Unix;
(* IMPORT OSError; *)

PROCEDURE SymLink(name1, name2: Pathname.T) (* RAISES {OSError.E} *) =
  BEGIN
    VAR
      s1 := M3toC.CopyTtoS(name1); s2 := M3toC.CopyTtoS(name2);
    BEGIN
      TRY
        IF Unix.symlink(s1,s2) # 0 THEN
          (* already exists? *)
  
          (*
          VAR
            buf : ARRAY[0..999] OF CHAR;
          BEGIN
            IF Unix.readlink (M3toC.TtoS(name2), ADR(buf), 1000) # 0 THEN
              RAISE OSError.E(NIL);
            END
          END;
  
          Why isn't this working??
..        *)
        END
      FINALLY
        M3toC.FreeCopiedS(s1); M3toC.FreeCopiedS(s2)
      END
    END;
  END SymLink;

BEGIN
END UnixUtils.

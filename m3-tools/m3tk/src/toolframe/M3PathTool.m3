(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3PathTool;

IMPORT M3Args, Rd, FileRd, OSError, Atom, Err, Thread, Fmt;
IMPORT M3Extension, M3FindFile, M3PathElem, M3PathElemOS, M3PathElemList,
       M3DirFindFile;

CONST
  Version = "9-Apr-93";

VAR
  tool_g := M3Args.New("m3pathtool", "search path tool", Version);

PROCEDURE Check(exts := M3Extension.All): M3FindFile.T RAISES {}=
  VAR path, tfile: REF ARRAY OF TEXT;
      dirs: M3PathElemList.T := NIL;
      dfinder, result: M3DirFindFile.Finder := NIL;
      tfinders: REF ARRAY OF M3DirFindFile.Finder := NIL;
      rd: Rd.T;
  BEGIN
    IF M3Args.Find(tool_g) THEN
      tfile := M3Args.GetPrefix(tool_g, TFile_Arg);
      IF tfile # NIL THEN
        tfinders := NEW(REF ARRAY OF M3DirFindFile.Finder, NUMBER(tfile^));
        FOR i := 0 TO NUMBER(tfile^)-1 DO tfinders[i] := NIL END;
        FOR i := 0 TO NUMBER(tfile^)-1 DO
          TRY
            rd := FileRd.Open(tfile[i]);
            TRY 
              tfinders[i] := NEW(M3DirFindFile.TFinder).init(exts, rd);
            FINALLY
              <*FATAL Rd.Failure, Thread.Alerted*>
              BEGIN Rd.Close(rd); END;
            END;
          EXCEPT
          | OSError.E(e) =>
              Err.Print(Fmt.F("error opening '%s' - %s", tfile[i],
                  Atom.ToText(e.head)), Err.Severity.Error);
          END;
        END;
      END;

      path := M3Args.GetPrefix(tool_g, Define_Arg);
      IF path # NIL THEN
        FOR i := 0 TO NUMBER(path^)-1 DO
          WITH p = path[i] DO
            dirs := M3PathElemList.AppendD(dirs,
                                             M3PathElemOS.DecomposePath(p));
          END;
        END; (* for *)
      END;

(*
      IF NOT M3Args.GetFlag(tool_g, NOSTD_Arg) THEN
        VAR pub := M3Config.Pub();
        BEGIN
          WHILE pub # NIL DO
            dirs := M3PathDefaults.Add(dirs,
                        M3PathElem.FromText(pub.head, pub.head));
            pub := pub.tail;
          END;
        END;
      END;
*)
      <*FATAL OSError.E*>
      BEGIN
        dfinder := NEW(M3DirFindFile.Finder).init(exts, dirs, NIL,
                       NEW(ErrorHandler));
      END;

      result := dfinder;
      (* merge *)
      IF tfinders # NIL THEN
        VAR
          start := NUMBER(tfinders^) - 1;
        BEGIN
          IF result = NIL THEN 
            REPEAT
              result := tfinders[start]; DEC(start);
            UNTIL result # NIL OR start < 0
          END;
          WHILE start >= 0 DO
            result := NEW(M3DirFindFile.Finder).merge(result, tfinders[start]);
            DEC(start);
          END;
        END
      END
    END; (* if *)
    RETURN result;
  END Check;

TYPE
  ErrorHandler = M3DirFindFile.ErrorHandler OBJECT
  OVERRIDES
    callback := ErrorProc;
  END;

PROCEDURE ErrorProc(<*UNUSED*> t: ErrorHandler; dir: M3PathElem.T;
                    ec: OSError.Code): BOOLEAN=
  BEGIN
    Err.Print(Fmt.F("error opening '%s' - %s", dir.text(),
                    Atom.ToText(ec.head)), Err.Severity.Error);
    RETURN TRUE;
  END ErrorProc;

BEGIN
  M3Args.RegisterPrefix(tool_g, Define_Arg,
    "specify an explicit path to be used for file searching");
  M3Args.RegisterPrefix(tool_g, TFile_Arg,
    "specify a file with an explicit map to be used for file searching");
  M3Args.RegisterFlag(tool_g, NOSTD_Arg,
    "suppress default scan of standard libraries");
END M3PathTool.

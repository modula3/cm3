(* distilled from RTLinker
 * Some versions of compiler optimize away this loop.
 *)
MODULE RTLink;
IMPORT RT0;

PROCEDURE FixImports (m: RT0.ModulePtr) =
  VAR imp: RT0.ImportPtr;
  BEGIN
    IF (m = NIL) THEN RETURN; END;
    imp := m.imports;
    WHILE (imp # NIL) DO
      IF (imp.import = NIL) THEN  imp.import := imp.binder (0);  END;
      imp := imp.next;
    END;
  END FixImports;

BEGIN
END RTLink.

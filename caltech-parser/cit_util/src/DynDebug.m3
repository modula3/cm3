MODULE DynDebug;
IMPORT Debug;

TYPE
  Rec = RECORD
    name  : TEXT;
    level : CARDINAL;
    res   : REF BOOLEAN;
    nxt   : REF Rec;
  END;

VAR
  mu := NEW(MUTEX);
  recs : REF Rec := NIL;
  
PROCEDURE DebugThis(mn : TEXT; lev : CARDINAL) : REF BOOLEAN =
  BEGIN
    LOCK mu DO
      WITH rec = NEW(REF Rec,
                     name  := mn,
                     level := lev,
                     res   := NEW(REF BOOLEAN),
                     nxt   := recs) DO
        recs := rec;
        Evaluate(rec^);
        RETURN rec.res
      END
    END
  END DebugThis;

PROCEDURE Evaluate(READONLY r : Rec) =
  BEGIN
    r.res^ := Debug.GetLevel() > r.level AND Debug.DebugThis(r.name)
  END Evaluate;

PROCEDURE Callback() =
  VAR
    p : REF Rec;
  BEGIN
    LOCK mu DO
      p := recs;
      WHILE p # NIL DO
        Evaluate(p^);
        p := p.nxt
      END
    END
  END Callback;
  
BEGIN
  Debug.RegisterCallback(Callback)
END DynDebug.

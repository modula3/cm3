(* $Id$ *)

MODULE NodeNameSplitter;
IMPORT Text;

REVEAL
  T = Public BRANDED Brand OBJECT
    name : TEXT;
    splitAt : INTEGER;
    length : CARDINAL;
  OVERRIDES
    init := Init;
    next := Next;
  END;

PROCEDURE Init(self : T; name : TEXT) : T =
  BEGIN
    self.name := name;
    self.splitAt := Text.Length(name);
    RETURN self
  END Init;

PROCEDURE Next(self : T; VAR cellName, nodeName : TEXT) : BOOLEAN =
  BEGIN
    IF self.splitAt < -1 THEN 
      RETURN FALSE 
    ELSIF self.splitAt = -1 THEN
      cellName := "";
      nodeName := self.name;
      self.splitAt := -2; (* done *)
      RETURN TRUE
    END;

    cellName := Text.Sub(self.name, 0, self.splitAt);
    nodeName := Text.Sub(self.name, self.splitAt + 1);
    self.splitAt := Text.FindCharR(self.name, '.', self.splitAt - 1);
    RETURN TRUE
  END Next;

BEGIN END NodeNameSplitter.
    

(*--------------------------------------------------------------------------*)
MODULE StdDepGraphNode;

IMPORT Text;

(*--------------------------------------------------------------------------*)
PROCEDURE New(n : TEXT; c : TEXT := NIL; pseudo := FALSE) : T =
  BEGIN
    RETURN NEW(T).init(n, c, pseudo);
  END New;

(*--------------------------------------------------------------------------*)
PROCEDURE Init(self : T; n : TEXT; c : TEXT := NIL; pseudo := FALSE) : T =
  BEGIN
    self.mName := n;
    self.mCmd := c;
    self.mHit := FALSE;
    self.mUpd := FALSE;
    self.mPhony := pseudo;
    RETURN self;
  END Init;

(*--------------------------------------------------------------------------*)
PROCEDURE Reset(self : T) =
  BEGIN
    self.mHit := FALSE;
    self.mUpd := FALSE;
  END Reset;

(*--------------------------------------------------------------------------*)
PROCEDURE Seen(self : T) =
  BEGIN
    self.mHit := TRUE;
  END Seen;

(*--------------------------------------------------------------------------*)
PROCEDURE Touch(self : T) =
  BEGIN
    self.mUpd := TRUE;
  END Touch;

(*--------------------------------------------------------------------------*)
PROCEDURE Name(self : T) : TEXT =
  BEGIN
    RETURN self.mName;
  END Name;

(*--------------------------------------------------------------------------*)
PROCEDURE Action(self : T) : TEXT =
  BEGIN
    RETURN self.mCmd;
  END Action;

(*--------------------------------------------------------------------------*)
PROCEDURE Visited(self : T) : BOOLEAN =
  BEGIN
    RETURN self.mHit;
  END Visited;

(*--------------------------------------------------------------------------*)
PROCEDURE Updated(self : T) : BOOLEAN =
  BEGIN
    RETURN self.mUpd;
  END Updated;

(*--------------------------------------------------------------------------*)
PROCEDURE Phony(self : T) : BOOLEAN =
  BEGIN
    RETURN self.mPhony;
  END Phony;

(*--------------------------------------------------------------------------*)
PROCEDURE Hash (node : T) : CARDINAL =
  VAR
    w := Text.Hash(node.mName);
  BEGIN
    IF w >= 0 THEN
      RETURN VAL(w, CARDINAL);
    ELSE
      RETURN VAL(-w, CARDINAL);
    END;
  END Hash;

(*--------------------------------------------------------------------------*)
PROCEDURE Equal (n1, n2 : T) : BOOLEAN =
  BEGIN
    RETURN Text.Equal(n1.mName, n2.mName);
  END Equal;

(*--------------------------------------------------------------------------*)
PROCEDURE Compare (n1, n2 : T) : [-1..1] =
  BEGIN
    RETURN Text.Compare(n1.mName, n2.mName);
  END Compare;

(*--------------------------------------------------------------------------*)
BEGIN
END StdDepGraphNode. 

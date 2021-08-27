(* Copyright (C) 2021 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE MODULE Json;

IMPORT IO, Fmt, Rd, Text, Thread;
IMPORT SortedTextRefTbl AS Tbl;
IMPORT Scanner AS SK;
IMPORT TextUtils,TextSeq,ASCII;

CONST
   MaxDepth = 1000;
   Root = "root";
   Obj  = "_Obj_";
   Arr  = "_Arr_";
   Null = "null";

CONST
  E_Comma  = 1;
  E_Eof    = 2;
  E_Object = 3;
  E_Text   = 4;
  E_Colon  = 5;
  E_Depth  = 6;
  E_Parse  = 7;
  E_Add    = 8;

REVEAL
  T =
    Public BRANDED OBJECT
      depth: INTEGER := 0;
      tok  : SK.T;
      nodeName  : TEXT;
      nodeValue : REFANY;
      nodeKind  : NodeKind;
      parent: T;
      list  : Tbl.Default := NIL;
    METHODS
      parseObject (parent : T) RAISES {E} := ParseObject;
      parseArray  (parent : T) RAISES {E} := ParseArray;
      parse       () RAISES {E} := Parse;
      parseValues (node: T; name : TEXT): BOOLEAN RAISES {E} := ParseValues;
      addNode(parent : T; type : NodeKind; name,value : TEXT): T RAISES{E} := AddNode;
      error (n: INTEGER) RAISES {E} := Error;
      struct (): BOOLEAN := StructNode;
      update (name,value : TEXT; nodeKind : NodeKind) := Update;
      doFormat(node : T; indent: TEXT): TEXT := DoFormat;
    OVERRIDES
      name       := Name;
      value      := Value;
      kind       := Kind;
      format     := Format;
      find       := Find;
      addText    := AddText;
      addInt     := AddInt;
      addFloat   := AddFloat;
      addBool    := AddBool;
      addNull    := AddNull;
      addObj     := AddObj;
      addArr     := AddArr;
      updateText := UpdateText;
      updateInt  := UpdateInt;
      updateBool := UpdateBool;
      updateFloat:= UpdateFloat;
      updateArrObj := UpdateArrObj;
      delete     := Delete;
      clear      := Clear;
      iterate    := Iterate;
      root       := GetRoot;
    END;

TYPE
  DefaultIterator = Iterator BRANDED OBJECT
    iter : Tbl.Iterator;
  OVERRIDES
    next := Next;
  END;

PROCEDURE Error (self: T; n: INTEGER) RAISES {E} =
  BEGIN
    IO.Put("Error " & Fmt.Int(n) & " ");
    IF self.tok # NIL THEN
      IF self.tok.token = SK.TK_Error THEN
        IO.Put(" : token error " & self.tok.msg & " ");
      END;
      IO.Put(" - line " & Fmt.Int(self.tok.line) & " offset "
             & Fmt.Int(self.tok.offset) & "\n");
    END;
    RAISE E;
  END Error;

PROCEDURE AddNode(self : T; parent : T; type : NodeKind; name,value : TEXT): T RAISES{E} =
  VAR
    node : T;
    nodeName : TEXT;
    size : CARDINAL;
  BEGIN
    IF parent.struct() THEN 
      IF parent.list = NIL THEN
        parent.list := NEW(Tbl.Default).init();
      END;
      size := parent.list.size();
      IF parent.nodeKind = NodeKind.nkArray THEN
        nodeName := Fmt.Int(size);
      ELSE
        nodeName := name;
      END;

      node := NEW(T);
      node.nodeKind := type;
      node.nodeName := nodeName;
      node.nodeValue := value;
      node.parent := parent;
      EVAL parent.list.put(nodeName, node);
    ELSIF parent.nodeKind = NodeKind.nkRoot THEN
      parent.nodeKind := type;
      parent.nodeValue := value;
      RETURN parent;
    ELSE
      self.error(E_Add);
    END;
    RETURN node;
  END AddNode;

PROCEDURE ParseValues (self: T; parent: T; name : TEXT): BOOLEAN RAISES {E} =
  VAR
    node : T;
    value : TEXT;
  BEGIN
    value := self.tok.toText();
    CASE self.tok.token OF
    | SK.TK_L_brace =>
        node := self.addNode(parent, NodeKind.nkObject, name, Obj);
        self.parseObject(node);
    | SK.TK_L_bracket =>
        node := self.addNode(parent, NodeKind.nkArray, name, Arr);
        self.parseArray(node);
    | SK.TK_Null =>
        EVAL  self.addNode(parent, NodeKind.nkNull, name, value);
    | SK.TK_True, SK.TK_False =>
        EVAL  self.addNode(parent, NodeKind.nkBool, name, value);
    | SK.TK_Card_const =>
        EVAL  self.addNode(parent, NodeKind.nkInt, name, value);
    | SK.TK_Real_const =>
        EVAL  self.addNode(parent, NodeKind.nkFloat, name, value);
    | SK.TK_Text_const =>
        EVAL  self.addNode(parent, NodeKind.nkText, name, value);
    ELSE
      RETURN FALSE;
    END;
    RETURN TRUE;
  END ParseValues;

PROCEDURE ParseObject (self: T; parent : T) RAISES {E} =
  VAR name : TEXT;
  BEGIN
    INC(self.depth);
    IF self.depth > MaxDepth THEN self.error(E_Depth); END;

    self.tok.next();
    WHILE self.tok.token # SK.TK_EOF DO
      CASE self.tok.token OF
      | SK.TK_Text_const => name := self.tok.toText();
      | SK.TK_R_brace => DEC(self.depth); RETURN;
      ELSE
        self.error(E_Text);
      END;

      self.tok.next();
      IF self.tok.token # SK.TK_Colon THEN self.error(E_Colon); END;

      self.tok.next();
      IF NOT self.parseValues(parent,name) THEN self.error(E_Parse); END;

      self.tok.next();
      IF self.tok.token = SK.TK_Comma THEN
        self.tok.next();
      ELSIF self.tok.token = SK.TK_R_brace THEN
        DEC(self.depth);
        RETURN;
      ELSE
        self.error(E_Comma);
      END;
    END;
    self.error(E_Eof);
  END ParseObject;

PROCEDURE ParseArray (self: T; parent : T) RAISES {E} =
  VAR res : BOOLEAN;
  BEGIN
    INC(self.depth);
    IF self.depth > MaxDepth THEN self.error(E_Depth); END;

    self.tok.next();
    WHILE self.tok.token # SK.TK_EOF DO
      res := self.parseValues(parent,"");
      IF res = FALSE THEN
        IF self.tok.token = SK.TK_R_bracket THEN DEC(self.depth); RETURN; END;
        self.error(E_Parse);
      END;

      self.tok.next();
      IF self.tok.token = SK.TK_Comma THEN
        self.tok.next();
        IF self.tok.token = SK.TK_R_bracket OR
           self.tok.token = SK.TK_R_brace THEN
          self.error(E_Object);
        END;
      ELSIF self.tok.token = SK.TK_R_bracket THEN
        DEC(self.depth);
        RETURN;
      ELSE
        self.error(E_Comma);
      END;
    END;
    self.error(E_Eof);
  END ParseArray;

PROCEDURE Parse (self: T) RAISES {E} =
  BEGIN
    self.tok.next();
    self.nodeName := Root;
    self.nodeKind := NodeKind.nkRoot;
    IF NOT self.parseValues(self,Root) THEN self.error(E_Parse); END;
    self.tok.next();
    IF self.tok.token # SK.TK_EOF THEN self.error(E_Eof); END;
  END Parse;

PROCEDURE ParseBuf (buf: TEXT) : T RAISES{E} =
  VAR
    s    : SK.Default;
    chars: SK.Buf;
    len  : CARDINAL;
    node : T := NEW(T);
  BEGIN
    len := Text.Length(buf);
    chars := NEW(SK.Buf, len+1);
    Text.SetChars(chars^, buf);
    chars[len] := '\000';
    s := NEW(SK.Default);
    node.tok := s.initFromBuf(chars);
    node.parse();
    RETURN node;
  END ParseBuf;

PROCEDURE ParseFile (f: TEXT) : T RAISES{E} =
  VAR rd : Rd.T;
      node : T;
  BEGIN
    TRY
      rd := IO.OpenRead(f);
      node := ParseStream(rd);
      Rd.Close(rd);
    EXCEPT
    | Rd.Failure, Thread.Alerted =>
    END;
    RETURN node;
  END ParseFile;

PROCEDURE ParseStream (rd : Rd.T) : T RAISES{E} =
  VAR
    s : SK.Default;
    node : T;
  BEGIN
    node := NEW(T);
    s := NEW(SK.Default);
    node.tok := s.initFromRd(rd);
    node.parse();
    RETURN node;
  END ParseStream;

PROCEDURE Format(self : T) : TEXT =
  BEGIN
    RETURN self.doFormat(self, "");
  END Format;

PROCEDURE DoFormat(self : T; node : T; indent: TEXT): TEXT =
  VAR
    prefix,res,key : TEXT;
    value : REFANY;

  PROCEDURE DoItems(): TEXT =
  VAR
    anode : T;
    count, i : INTEGER;
    s, res : TEXT;
    iter : Tbl.Iterator;
  BEGIN
    IF node.list = NIL OR node.list.size() = 0 THEN
      RETURN " ";
    END;
    res := "\n";
    count := node.list.size() - 1;
    s := indent & "    ";
    i := 0;
    iter := node.list.iterateOrdered(TRUE);
    WHILE iter.next(key,value) DO
      anode := NARROW(value,T);
      res := res & self.doFormat(anode,s);
      IF i < count THEN
        res := res & ",\n";
      ELSE
        res := res & "\n" & indent;
      END;
      INC(i);
    END;
    RETURN res;
  END DoItems;

  BEGIN
    res := "";
    IF node.parent # NIL AND
       node.parent.nodeKind = NodeKind.nkObject THEN
      prefix := "\"" & node.nodeName & "\"" & ": ";
    ELSE
      prefix := "";
    END;
    CASE node.nodeKind OF
    | NodeKind.nkObject => res := indent & prefix & "{" & DoItems() & "}";
    | NodeKind.nkArray => res := indent & prefix & "[" & DoItems() & "]";
    | NodeKind.nkText => res := indent & prefix & "\"" & node.nodeValue & "\"";
    ELSE
      res := indent & prefix & node.nodeValue;
    END;
    RETURN res;
  END DoFormat;

PROCEDURE Name(self : T) : TEXT =
  BEGIN
    RETURN self.nodeName;
  END Name;

PROCEDURE Value(self : T) : TEXT =
  BEGIN
    IF self.nodeKind = NodeKind.nkArray THEN
      RETURN Arr;
    ELSIF self.nodeKind = NodeKind.nkObject THEN
      RETURN Obj;
    ELSE
      RETURN self.nodeValue;
    END;
  END Value;

PROCEDURE Kind(self : T) : NodeKind =
  BEGIN
    RETURN self.nodeKind;
  END Kind;

PROCEDURE Match(VAR node : T; item : TEXT) : BOOLEAN =
  VAR
    iter : Tbl.Iterator;
    key : TEXT;
    value : REFANY;
  BEGIN
    iter := node.list.iterateOrdered(TRUE);
    WHILE iter.next(key,value) DO
      IF Text.Equal(item,key) THEN
        IF node.struct() THEN
          node := value;
        END;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END Match;

PROCEDURE Find(self : T; path : TEXT) : T =
  VAR
    seq : TextSeq.T;
    item : TEXT;
    res : BOOLEAN := TRUE;
    node : T := self;
  BEGIN
    seq := TextUtils.Tokenize(path,ASCII.Set{'/'});
    IF seq.size() = 0 THEN 
      IF Text.Equal(path,"/") THEN RETURN self.root(); END;
      RETURN NIL;
    END;
    FOR i := 0 TO seq.size() - 1 DO
      item := seq.get(i);
      IF NOT Match(node,item) THEN
        res := FALSE;
        EXIT;
      END;
    END;
    IF res THEN RETURN node; ELSE RETURN NIL; END;
  END Find;

PROCEDURE StructNode(self : T) : BOOLEAN =
  BEGIN
    IF (self.nodeKind = NodeKind.nkArray OR
        self.nodeKind = NodeKind.nkObject) THEN
     RETURN TRUE;
   ELSE
     RETURN FALSE;
   END;
  END StructNode;

PROCEDURE AddText(self : T; name,value : TEXT) : T =
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    RETURN self.addNode(self, NodeKind.nkText, name, value ); <*NOWARN*> 
  END AddText;

PROCEDURE AddInt(self : T; name : TEXT; value : INTEGER) : T =
  VAR val : TEXT;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    val := Fmt.Int(value);
    RETURN self.addNode(self, NodeKind.nkInt, name, val); <*NOWARN*>
  END AddInt;

PROCEDURE AddFloat(self : T; name : TEXT; value : LONGREAL) : T =
  VAR val : TEXT;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    val := Fmt.LongReal(value);
    RETURN self.addNode(self, NodeKind.nkFloat, name, val); <*NOWARN*>
  END AddFloat;

PROCEDURE AddBool(self : T; name : TEXT; value : BOOLEAN) : T =
  VAR val : TEXT;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    val := Fmt.Bool(value);
    RETURN self.addNode(self, NodeKind.nkBool, name, val); <*NOWARN*>
  END AddBool;

PROCEDURE AddNull(self : T; name : TEXT) : T =
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    RETURN self.addNode(self, NodeKind.nkNull, name, Null); <*NOWARN*>
  END AddNull;

PROCEDURE AddObj(self : T; name : TEXT; obj : T := NIL) : T =
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END; 
    IF obj = NIL THEN
      (* just add an empty obj *)
      RETURN self.addNode(self, NodeKind.nkObject, name, Obj); <*NOWARN*>
    ELSE
      IF self.list # NIL AND obj.nodeKind = NodeKind.nkObject THEN
        IF NOT self.list.put(name,obj) THEN
          RETURN obj;
        END;
      END;
      RETURN NIL;
    END;
  END AddObj;

PROCEDURE AddArr(self : T; name : TEXT; arr : T := NIL) : T =
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END; 
    IF arr = NIL THEN
      (* just add an empty array *)
      RETURN self.addNode(self, NodeKind.nkArray, name, Arr); <*NOWARN*>
    ELSE
      IF self.list # NIL AND arr.nodeKind = NodeKind.nkArray THEN
        IF NOT self.list.put(name,arr) THEN
          RETURN arr;
        END;
      END;
      RETURN NIL;
    END;
  END AddArr;

PROCEDURE Update(self : T; name,value : TEXT; nodeKind : NodeKind) =
  VAR 
    node : T;
    val : REFANY;
  BEGIN
    IF self.list.get(name,val) THEN
      node := NARROW(val,T);
      IF node.nodeKind = nodeKind THEN
        node.nodeValue := value;
      END;
    END;
  END Update;

PROCEDURE UpdateText(self : T; name,value : TEXT) =
  BEGIN
    self.update(name,value,NodeKind.nkText);
  END UpdateText;

PROCEDURE UpdateInt(self : T; name : TEXT; value : INTEGER) =
  BEGIN
    self.update(name,Fmt.Int(value),NodeKind.nkInt);
  END UpdateInt;

PROCEDURE UpdateBool(self : T; name : TEXT; value : BOOLEAN) =
  BEGIN
    self.update(name,Fmt.Bool(value),NodeKind.nkBool);
  END UpdateBool;

PROCEDURE UpdateFloat(self : T; name : TEXT; value : LONGREAL) =
  BEGIN
    self.update(name,Fmt.LongReal(value),NodeKind.nkFloat);
  END UpdateFloat;

PROCEDURE UpdateArrObj(self : T; name : TEXT; value : T) =
  VAR 
    val : REFANY;
  BEGIN
    IF value = NIL THEN RETURN; END;
    IF NOT (self.struct() AND value.struct()) THEN RETURN; END; 
    IF self.list # NIL THEN
      IF self.list.get(name,val) THEN
        EVAL self.list.put(name,value);
      END;
    END;
  END UpdateArrObj;

PROCEDURE Delete(self : T; name : TEXT) : T =
  VAR 
    value : REFANY;
    node : T := NIL;
    res : BOOLEAN;
  BEGIN
    IF self.list # NIL THEN
      res := self.list.delete(name,value);
      IF res THEN
        node := NARROW(value,T);
      END;
    END;
    RETURN node;
  END Delete;

PROCEDURE Clear(self : T) =
  BEGIN
    IF self.struct() THEN 
      self.list := NIL;
    END;
  END Clear;

PROCEDURE GetRoot(self : T) : T =
  VAR node : T := self;
  BEGIN
    WHILE node.parent # NIL DO
      node := node.parent;
    END;
    RETURN node;
  END GetRoot;

PROCEDURE Iterate(self : T) : Iterator =
  VAR
    iter : DefaultIterator;
    s : Tbl.Iterator;
  BEGIN
    (* only iterate over obj or array *)
    IF NOT self.struct() THEN RETURN NIL; END;
    s := self.list.iterateOrdered(TRUE);
    iter := NEW(DefaultIterator, iter := s);
    RETURN iter;
  END Iterate;

PROCEDURE Next(self : DefaultIterator; VAR name : TEXT; VAR value : T) : BOOLEAN =
  VAR node : REFANY;
  res : BOOLEAN;
  BEGIN
    res := self.iter.next(name,node);
    value := node;
    RETURN res;
  END Next;

BEGIN
END Json.

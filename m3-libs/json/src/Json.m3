UNSAFE MODULE Json;

IMPORT IO, Fmt, Rd, Text, Thread;
IMPORT SortedTextRefTbl AS Tbl;
IMPORT Scanner AS SK;
IMPORT TextUtils,TextSeq,ASCII;

EXCEPTION E;

CONST
   MaxDepth = 1000;
   Root = "root";
   Obj = "_obj_";
   Arr = "_Arr_";

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
      name  : TEXT;
      nodeKind  : NodeKind;
      nodeValue : REFANY;
      parent: T;
      list  : Tbl.Default := NIL;
    METHODS
      parseObject (parent : T) RAISES {E} := ParseObject;
      parseArray  (parent : T) RAISES {E} := ParseArray;
      parse       () RAISES {E} := Parse;
      parseValues (node: T; name : TEXT): BOOLEAN RAISES {E} := ParseValues;
      addNode(parent : T; type : NodeKind; name,value : TEXT): T RAISES{E} := AddNode;
      error (n: INTEGER) RAISES {E} := Error;
      doFormat(node : T; indent: TEXT): TEXT := DoFormat;
    OVERRIDES
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
      delete     := Delete;
      clear      := Clear;
      root       := GetRoot;
    END;

PROCEDURE Error (self: T; n: INTEGER) RAISES {E} =
  BEGIN
    IO.Put("Error " & Fmt.Int(n) & " ");
    IF self.tok.token = SK.TK_Error THEN
      IO.Put(" : token error " & self.tok.msg & " ");
    END;
    IO.Put(" - line " & Fmt.Int(self.tok.line) & " offset "
             & Fmt.Int(self.tok.offset) & "\n");
    RAISE E;
  END Error;

PROCEDURE AddNode(self : T; parent : T; type : NodeKind; name,value : TEXT): T RAISES{E} =
  VAR
    node : T;
    nodeName : TEXT;
    size : CARDINAL;
  BEGIN
    IF parent.nodeKind = NodeKind.nkObject OR
       parent.nodeKind = NodeKind.nkArray THEN
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
      node.name := nodeName;
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
    self.name := Root;
    self.nodeKind := NodeKind.nkRoot;
    IF NOT self.parseValues(self,Root) THEN self.error(E_Parse); END;
    self.tok.next();
    IF self.tok.token # SK.TK_EOF THEN self.error(E_Eof); END;
  END Parse;

PROCEDURE ParseBuf (buf: TEXT) : T =
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
    TRY node.parse(); EXCEPT | E => END;
    RETURN node;
  END ParseBuf;

PROCEDURE ParseFile (f: TEXT) : T =
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

PROCEDURE ParseStream (rd : Rd.T) : T =
  VAR
    s : SK.Default;
    node : T;
  BEGIN
    node := NEW(T);
    s := NEW(SK.Default);
    node.tok := s.initFromRd(rd);
    TRY
      node.parse();
    EXCEPT
    | E =>
    END;
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
      prefix := "\"" & node.name & "\"" & ": ";
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

PROCEDURE Value(self : T) : TEXT =
  BEGIN
    IF (self.nodeKind = NodeKind.nkArray OR
        self.nodeKind = NodeKind.nkObject) THEN
      RETURN "";
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
        IF node.nodeKind = NodeKind.nkArray OR
           node.nodeKind = NodeKind.nkObject THEN
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
    IF seq.size() = 0 THEN RETURN NIL; END;
    FOR i := 0 TO seq.size() - 1 DO
      item := seq.get(i);
      IF NOT Match(node,item) THEN
        res := FALSE;
        EXIT;
      END;
    END;
    IF res THEN
     IO.Put("path found\n" & node.name & " " & node.nodeValue);
     RETURN node;
    ELSE
      IO.Put("path not found\n");
      RETURN NIL;
    END;
  END Find;

PROCEDURE AddText(self : T; name,value : TEXT) : T =
  BEGIN
    RETURN self.addNode(self, NodeKind.nkText, name, value );
  END AddText;

PROCEDURE AddInt(self : T; name : TEXT; value : INTEGER) : T =
  VAR val : TEXT;
  BEGIN
    val := Fmt.Int(value);
    RETURN self.addNode(self, NodeKind.nkInt, name, val);
  END AddInt;

PROCEDURE AddFloat(self : T; name : TEXT; value : LONGREAL) : T =
  VAR val : TEXT;
  BEGIN
    val := Fmt.LongReal(value);
    RETURN self.addNode(self, NodeKind.nkFloat, name, val);
  END AddFloat;

PROCEDURE AddBool(self : T; name : TEXT; value : BOOLEAN) : T =
  VAR val : TEXT;
  BEGIN
    val := Fmt.Bool(value);
    RETURN self.addNode(self, NodeKind.nkBool, name, val);
  END AddBool;

PROCEDURE AddNull(self : T; name : TEXT) : T =
  BEGIN
    RETURN self.addNode(self, NodeKind.nkNull, name,"null");
  END AddNull;

(* think some confusion between adding an object and adding a node i
add an object can only be object parm and has to be added to 
obj or array adding a node can be any type but has to be to 
object or array maybe call it addnode. Also this should be a copy
since if added twice is problem *)

PROCEDURE AddObj(self : T; name : TEXT; obj : T := NIL) : T =
  BEGIN
    IF obj = NIL THEN
      RETURN self.addNode(self, NodeKind.nkObject, name, Obj);
    ELSE
      IF (self.nodeKind = NodeKind.nkArray OR
          self.nodeKind = NodeKind.nkObject) AND 
        self.list # NIL AND obj.nodeKind = NodeKind.nkObject THEN
        IF self.list.put(name,obj) THEN
          RETURN obj;
        ELSE
          RETURN NIL;
        END;
      END;
      RETURN NIL;
    END;
  END AddObj;

PROCEDURE AddArr(self : T; name : TEXT) : T =
  BEGIN
    RETURN self.addNode(self, NodeKind.nkArray, name, Arr);
  END AddArr;

(* this needs think since can update node to anything ?? *)
PROCEDURE UpdateText(self : T; name,value : TEXT) =
  VAR 
    node : T;
    val : REFANY;
  BEGIN
    IF self.list.get(name,val) THEN
      node := NARROW(val,T);
      IF node.nodeKind = NodeKind.nkText THEN
        node.nodeValue := value;
      END;
    END;
  END UpdateText;

PROCEDURE UpdateInt(self : T; name : TEXT; value : INTEGER) =
  VAR 
    node : T;
    val : REFANY;
  BEGIN
    IF self.list.get(name,val) THEN
      node := NARROW(val,T);
      IF node.nodeKind = NodeKind.nkInt THEN
        node.nodeValue := Fmt.Int(value);
      END;
    END;
  END UpdateInt;

PROCEDURE UpdateBool(self : T; name : TEXT; value : BOOLEAN) =
  VAR 
    node : T;
    val : REFANY;
  BEGIN
    IF self.list.get(name,val) THEN
      node := NARROW(val,T);
      IF node.nodeKind = NodeKind.nkInt THEN
        node.nodeValue := Fmt.Bool(value);
      END;
    END;
  END UpdateBool;

PROCEDURE UpdateFloat(self : T; name : TEXT; value : LONGREAL) =
  VAR 
    node : T;
    val : REFANY;
  BEGIN
    IF self.list.get(name,val) THEN
      node := NARROW(val,T);
      IF node.nodeKind = NodeKind.nkInt THEN
        node.nodeValue := Fmt.LongReal(value);
      END;
    END;
  END UpdateFloat;

PROCEDURE Delete(self : T; name : TEXT) : T =
  VAR 
    value : REFANY;
    node : T := NIL;
    res : BOOLEAN;
  BEGIN
    IF (* (self.nodeKind = NodeKind.nkArray OR
        self.nodeKind = NodeKind.nkObject) AND*) self.list # NIL THEN
      res := self.list.delete(name,value);
      IF res THEN
        node := NARROW(value,T);
      END;
    END;
    RETURN node;
  END Delete;

PROCEDURE Clear(self : T) =
  BEGIN
    IF (self.nodeKind = NodeKind.nkArray OR
        self.nodeKind = NodeKind.nkObject) THEN
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

BEGIN
END Json.

(* Copyright (C) 2021 Peter McKinna. All rights reserved. *)
(* See file COPYRIGHT-BSD for details. *)

UNSAFE MODULE Json;

IMPORT IO, Fmt, Lex, FloatMode, Scan, Rd, Wr, Text, Thread;
IMPORT TextUtils, TextSeq, ASCII;
IMPORT SortedTextRefTbl AS Tbl;
IMPORT JsonScanner AS SK;

CONST
   MaxDepth = 1000;
   Root = "root";
   Obj  = "_Obj_";
   Arr  = "_Arr_";
   Null = "null";
   PathSep = '/';

CONST
  E_Comma  = 1;
  E_Eof    = 2;
  E_Object = 3;
  E_Text   = 4;
  E_Colon  = 5;
  E_Depth  = 6;
  E_Parse  = 7;
  E_Add    = 8;
  E_Type   = 9;

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
      addNode(parent : T; type : NodeKind;
              name,value : TEXT): T RAISES{E} := AddNode;
      error (n: INTEGER) RAISES {E} := Error;
      struct (): BOOLEAN := StructNode;
      update (name,value : TEXT; nodeKind : NodeKind) := Update;
      doFormat(node : T; indent: TEXT; pretty : BOOLEAN): TEXT := DoFormat;
    OVERRIDES
      name       := Name;
      value      := Value;
      kind       := Kind;
      size       := Size;
      format     := Format;
      rawText    := RawText;
      find       := Find;
      copy       := Copy;
      getInt     := GetInt;
      getFloat   := GetFloat;
      getBool    := GetBool;
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

VAR
  nullObj := ParseBuf("{}"); <*NOWARN*>

PROCEDURE Error (self: T; n: INTEGER) RAISES {E} =
  BEGIN
    IO.Put("Error " & Fmt.Int(n) & " ");
    IF self.tok # NIL THEN
      IF self.tok.token = SK.TK_Error THEN
        IO.Put(" : token error " & self.tok.msg & " ");
      END;
      IO.Put(" - line " & Fmt.Int(self.tok.line) & " offset "
             & Fmt.Int(self.tok.offset) & Wr.EOL);
    END;
    RAISE E;
  END Error;

PROCEDURE AddNode(self : T; parent : T;
                  type : NodeKind; name,value : TEXT): T RAISES{E} =
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
      (* here nodeName and key are identical and kept in sync *)
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

PROCEDURE ParseBuf (buf: TEXT) : T RAISES {E} =
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

PROCEDURE ParseArr (arr: REF ARRAY OF CHAR) : T RAISES{E} =
  VAR
    s    : SK.Default;
    node : T := NEW(T);
  BEGIN
    s := NEW(SK.Default);
    node.tok := s.initFromBuf(arr);
    node.parse();
    RETURN node;
  END ParseArr;

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

PROCEDURE RawText(self : T) : TEXT =
  BEGIN
    RETURN self.doFormat(self, "", FALSE);
  END RawText;

PROCEDURE Format(self : T) : TEXT =
  BEGIN
    RETURN self.doFormat(self, "", TRUE);
  END Format;

PROCEDURE DoFormat(self : T; node : T; indent: TEXT; pretty : BOOLEAN): TEXT =
  VAR
    prefix,res,key : TEXT;
    value : REFANY;

  PROCEDURE DoItems(): TEXT =
  CONST Indent = "    ";
  VAR
    anode : T;
    count, i : INTEGER;
    s, res : TEXT;
    iter : Tbl.Iterator;
  BEGIN
    IF node.list = NIL OR node.list.size() = 0 THEN
      RETURN " ";
    END;
    res := "";
    count := node.list.size() - 1;
    s := indent;
    IF pretty THEN res := Wr.EOL; s := s & Indent; END;
    i := 0;
    iter := node.list.iterateOrdered(TRUE);
    WHILE iter.next(key,value) DO
      anode := NARROW(value,T);
      res := res & self.doFormat(anode, s, pretty);
      IF i < count THEN
        res := res & ",";
        IF pretty THEN res := res & Wr.EOL; END;
      ELSE
        IF pretty THEN res := res & Wr.EOL & indent; END;
      END;
      INC(i);
    END;
    RETURN res;
  END DoItems;

  BEGIN
    res := "";
    IF node.parent # NIL AND
       node.parent.nodeKind = NodeKind.nkObject THEN
      prefix := indent & "\"" & node.nodeName & "\": ";
    ELSE
      prefix := indent;
    END;
    CASE node.nodeKind OF
    | NodeKind.nkObject => res := prefix & "{" & DoItems() & "}";
    | NodeKind.nkArray => res := prefix & "[" & DoItems() & "]";
    | NodeKind.nkText => res := prefix & "\"" & node.nodeValue & "\"";
    ELSE
      res := prefix & node.nodeValue;
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

PROCEDURE GetInt(self : T) : INTEGER RAISES {E} =
  VAR res : INTEGER;
  BEGIN
    IF self.nodeKind # NodeKind.nkInt THEN
      self.error(E_Type);
    END;
    TRY
      res := Scan.Int(self.nodeValue);
    EXCEPT
    | FloatMode.Trap,Lex.Error => self.error(E_Type);
    END;
    RETURN res;
  END GetInt;

PROCEDURE GetFloat(self : T) : LONGREAL RAISES {E} =
  VAR res : LONGREAL;
  BEGIN
    IF self.nodeKind # NodeKind.nkFloat THEN
      self.error(E_Type);
    END;
    TRY
      res := Scan.LongReal(self.nodeValue);
    EXCEPT
    | FloatMode.Trap,Lex.Error => self.error(E_Type);
    END;
    RETURN res;
  END GetFloat;

PROCEDURE GetBool(self : T) : BOOLEAN RAISES {E} =
  VAR res : BOOLEAN;
  BEGIN
    IF self.nodeKind # NodeKind.nkBool THEN
      self.error(E_Type);
    END;
    TRY
      res := Scan.Bool(self.nodeValue);
    EXCEPT
    | Lex.Error => self.error(E_Type);
    END;
    RETURN res;
  END GetBool;

PROCEDURE Kind(self : T) : NodeKind =
  BEGIN
    RETURN self.nodeKind;
  END Kind;

PROCEDURE Size(self : T) : CARDINAL =
  BEGIN
    IF self.struct() AND self.list # NIL THEN
      RETURN self.list.size();
    ELSE
      RETURN 0;
    END;
  END Size;

PROCEDURE Match(VAR node : T; item : TEXT) : BOOLEAN =
  VAR
    iter : Tbl.Iterator;
    key : TEXT;
    value : REFANY;
  BEGIN
    iter := node.list.iterateOrdered(TRUE);
    WHILE iter.next(key,value) DO
      IF Text.Equal(item, key) THEN
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
    seq := TextUtils.Tokenize(path,ASCII.Set{PathSep});
    IF seq.size() = 0 THEN 
      IF Text.Equal(path,Text.FromChar(PathSep)) THEN RETURN self.root(); END;
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

PROCEDURE CloneTree(self,parent : T) : T =
  VAR 
    node,newVal : T;
    key : TEXT;
    val : REFANY;
    iter : Tbl.Iterator;
  BEGIN
    IF self = NIL THEN RETURN NIL; END;

    node := NEW(T, nodeName := self.nodeName, nodeValue := self.nodeValue,
                nodeKind := self.nodeKind, parent := parent, list := NIL);

    IF self.list # NIL THEN
      node.list := NEW(Tbl.Default).init();
      iter := self.list.iterateOrdered(TRUE);
      WHILE iter.next(key,val) DO
        newVal := CloneTree(val,node);
        EVAL node.list.put(key, newVal);
      END;
    END;
    RETURN node;
  END CloneTree;

PROCEDURE Copy(self : T) : T =
  BEGIN
    RETURN CloneTree(self,NIL);
  END Copy;

PROCEDURE StructNode(self : T) : BOOLEAN =
  BEGIN
    IF (self.nodeKind = NodeKind.nkArray OR
        self.nodeKind = NodeKind.nkObject) THEN
     RETURN TRUE;
   ELSE
     RETURN FALSE;
   END;
  END StructNode;

PROCEDURE AddText(self : T; name,value : TEXT) : T RAISES {E} =
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    RETURN self.addNode(self, NodeKind.nkText, name, value ); 
  END AddText;

PROCEDURE AddInt(self : T; name : TEXT; value : INTEGER) : T RAISES {E} =
  VAR val : TEXT;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    val := Fmt.Int(value);
    RETURN self.addNode(self, NodeKind.nkInt, name, val);
  END AddInt;

PROCEDURE AddFloat(self : T; name : TEXT; value : LONGREAL) : T RAISES {E} =
  VAR val : TEXT;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    val := Fmt.LongReal(value);
    RETURN self.addNode(self, NodeKind.nkFloat, name, val);
  END AddFloat;

PROCEDURE AddBool(self : T; name : TEXT; value : BOOLEAN) : T RAISES {E} =
  VAR val : TEXT;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    val := TextUtils.Lower(Fmt.Bool(value));
    RETURN self.addNode(self, NodeKind.nkBool, name, val);
  END AddBool;

PROCEDURE AddNull(self : T; name : TEXT) : T RAISES {E} =
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END;
    RETURN self.addNode(self, NodeKind.nkNull, name, Null);
  END AddNull;

PROCEDURE AddObj(self : T; name : TEXT; obj : T := NIL) : T RAISES {E} =
  VAR node,copy : T;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END; 
    IF obj = NIL THEN
      (* just add an empty obj *)
      RETURN self.addNode(self, NodeKind.nkObject, name, Obj);
    ELSE
      IF obj.nodeKind = NodeKind.nkObject THEN
        IF self.nodeKind = NodeKind.nkArray THEN
          name := Fmt.Int(self.size());
        END;
        node := self.addNode(self, NodeKind.nkObject, name, Obj);
        copy := obj.copy();
        node.list := copy.list;
        RETURN node;
      END;
      RETURN NIL;
    END;
  END AddObj;

PROCEDURE AddArr(self : T; name : TEXT; arr : T := NIL) : T RAISES {E} =
  VAR node,copy : T;
  BEGIN
    IF NOT self.struct() THEN RETURN NIL; END; 
    IF arr = NIL THEN
      (* just add an empty array *)
      RETURN self.addNode(self, NodeKind.nkArray, name, Arr); <*NOWARN*>
    ELSE
      IF arr.nodeKind = NodeKind.nkArray THEN
        IF self.nodeKind = NodeKind.nkArray THEN
          name := Fmt.Int(self.size());
        END;
        node := self.addNode(self, NodeKind.nkArray, name, Arr); <*NOWARN*>
        copy := arr.copy();
        node.list := copy.list;
        RETURN node;
      END;
      RETURN NIL;
    END;
  END AddArr;

(* update non structured nodes *)
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
    ELSE
      (* treat name as a path *)
      node := self.find(name);
      IF node # NIL THEN
        IF node.nodeKind = nodeKind THEN
          node.nodeValue := value;
        END;
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
    self.update(name,TextUtils.Lower(Fmt.Bool(value)),NodeKind.nkBool);
  END UpdateBool;

PROCEDURE UpdateFloat(self : T; name : TEXT; value : LONGREAL) =
  BEGIN
    self.update(name,Fmt.LongReal(value),NodeKind.nkFloat);
  END UpdateFloat;

PROCEDURE GetLastNode(self : T; VAR name : TEXT) : T =
  VAR
    node : T;
    val : REFANY;
    seq : TextSeq.T;
  BEGIN
    IF self.list.get(name,val) THEN
      node := self;
    ELSE
      node := self.find(name);
      IF node # NIL THEN
        node := node.parent;
        seq := TextUtils.Tokenize(name,ASCII.Set{PathSep});
        IF seq.size() > 0 THEN
          name := seq.get(seq.size() - 1);
        ELSE
          node := NIL;
        END;
      END;
    END;
    RETURN node;
  END GetLastNode;

(* Update a node of any type with a structured node (array or obj)
   the original node will be deleted. Self is the target of the update
   and value is the source.
  There is a slight ambiguity between updates of simple items with
   those of structured, since with simple item the type of the target has
   to agree with the source whereas with structured the source isnt
   checked and is just overwritten *)
PROCEDURE UpdateArrObj(self : T; name : TEXT; value : T) =
  VAR 
    node,copy : T;
    val : REFANY;
  BEGIN
    IF value = NIL OR (NOT value.struct()) THEN RETURN; END;
    node := GetLastNode(self, name);
    IF node # NIL AND node.list # NIL THEN
      IF node.list.get(name, val) THEN
        IF node.nodeKind = NodeKind.nkArray THEN
          copy := value.copy();
          EVAL node.list.put(name, copy);
        ELSE
          node := node.addNode(node, NodeKind.nkObject, name, Obj);<*NOWARN*>
          copy := value.copy();
          node.list := copy.list;
        END;
      END;
    END;
  END UpdateArrObj;

PROCEDURE Delete(self : T; name : TEXT) : T =
  VAR 
    node,pn,an : T := NIL;
    value,val : REFANY;
    key,newKey : TEXT;
    iter : Tbl.Iterator;
    copy : Tbl.T;
    i : INTEGER;
    res : BOOLEAN;
  BEGIN
    IF self.list # NIL THEN
      res := self.list.delete(name,value);
      IF res THEN
        pn := self;
        node := NARROW(value,T);
      ELSE
        (* treat name as a path *)
        pn := GetLastNode(self,name);
        IF pn # NIL THEN
          res := pn.list.delete(name,value);
          IF res THEN
            node := NARROW(value,T);
          END;
        END;
      END;

      (* if this is an array we reorder the names *)
      IF pn # NIL AND pn.nodeKind = NodeKind.nkArray THEN
        iter := pn.list.iterateOrdered(TRUE);
        i := 0;
        copy := NEW(Tbl.Default).init();
        WHILE iter.next(key,val) DO
          EVAL copy.put(key,Fmt.Int(i));
          INC(i);
        END;
        iter := copy.iterateOrdered(TRUE);
        WHILE iter.next(key,val) DO
          newKey := NARROW(val,TEXT);
          EVAL pn.list.delete(key,value);
          an := NARROW(value,T);
          an.nodeName := newKey;
          EVAL pn.list.put(newKey,value);
        END;
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

(* debug procs *)
PROCEDURE Dump(node : T) =
  BEGIN
    IO.Put("Dump-\n" & node.nodeName & "\n");
    IO.Put("depth ");
    IO.PutInt(node.depth);
    IO.Put("\nkind ");
    IO.PutInt(ORD(node.nodeKind));
    IO.Put("\nvalue ");
    IO.Put(node.nodeValue & "\n");
    IO.Put("parent ");
    IO.PutInt(LOOPHOLE(node.parent,INTEGER));
    IO.Put("\n");
  END Dump;

PROCEDURE Validate(self : T) =
  VAR
    key : TEXT;
    val : REFANY;
    iter : Tbl.Iterator;
  BEGIN
    IF self = NIL THEN RETURN; END;
    Dump(self);
    IF self.list = NIL THEN IO.Put("No children\n"); RETURN; END;
    iter := self.list.iterateOrdered(TRUE);
    WHILE iter.next(key,val) DO
      Validate(val);
    END;
  END Validate;

BEGIN
END Json.

(* $Id$ *)

GENERIC MODULE PersistentTable(Elem, IntElemTbl);
IMPORT Pathname;
IMPORT IntTriple;
IMPORT Int_IntTripleTbl;
IMPORT Rd, Wr, OSError;
IMPORT Thread;
IMPORT FileRd, FileWr;
IMPORT Fmt, Text;
IMPORT IntPQ;
IMPORT Pickle;
IMPORT FS;
IMPORT Random;
IMPORT IntInt, IntIntArraySort;
IMPORT FileSharing;

<* FATAL Thread.Alerted *>

(* implementation of an ObjectStore is as follows:

   <IndexWidth><DLEN><DIRECTORY><DATA>

   <DATA> is written sequentially and compacted when it becomes more than
          half empty 

   <DIRECTORY> has format <key><start><length> to signify that <key> is 
   stored with offset <start> from beginning of file (including directory)
   and length <length> bytes

   an offset of zero means no data for that key

   each integer in directory has format SSSSSDDDD where S denotes ASCII
   space and D a decimal digit \in 0..9.  The lengths are fixed width
   of width IndexWidth

   <DLEN> denotes the total length of the directory (in records)

   <IndexWidth> and <DLEN> are written in the same format.

   For this code to work on windows, it is essential that rd and wr
   never be open simultaneously.  The invariant is that rd is open 
   unless it is NIL, and similarly for wr.  Every open must be preceded
   by a check that the other is closed, and that the open is necessary.
 *)

CONST IndexWidth = 10; (* large enough for 32-bit *)
CONST DirOffset = 2 * IndexWidth;

REVEAL
  T = Public BRANDED Brand OBJECT
    path : Pathname.T;
    dir : Int_IntTripleTbl.T;
    freeMap : IntPQ.T;
    rd : Rd.T;
    wr : Wr.T;
    dirSize : CARDINAL; (* size of directory segment in records *)
  METHODS
    resize(newSize : CARDINAL) RAISES { Wr.Failure, OSError.E, Rd.Failure, Error, Pickle.Error, Rd.EndOfFile } := Resize;
    
    reading() RAISES { OSError.E, Wr.Failure } := SetReading;
    writing() RAISES { OSError.E, Rd.Failure } := SetWriting;
  OVERRIDES 
    init := Init;
    putE := PutE;
    getE := GetE;
    deleteE := DeleteE;
    get := Get;
    put := Put;
    delete := Delete;

    iterate := Iterate;
    iterateE := IterateE;

    size := Size;
    keyEqual := KeyEqual;
    close := Close;
  END;

PROCEDURE Close(t : T) RAISES { Rd.Failure, Wr.Failure } = 
  BEGIN 
    IF t.rd # NIL THEN Rd.Close(t.rd); t.rd := NIL END;
    IF t.wr # NIL THEN Wr.Close(t.wr); t.wr := NIL END
  END Close;

VAR Windows := NOT FileSharing.SimultaneousReadersAndWritersAreOK;

PROCEDURE SetReading(t : T) RAISES { OSError.E, Wr.Failure } =
  BEGIN
    IF t.rd = NIL THEN 
      IF Windows AND t.wr # NIL THEN Wr.Close(t.wr); t.wr := NIL END;
      t.rd := FileRd.Open(t.path) 
    END
  END SetReading;

PROCEDURE SetWriting(t : T) RAISES { OSError.E, Rd.Failure } =
  BEGIN
    IF t.wr = NIL THEN 
      IF Windows AND t.rd # NIL THEN Rd.Close(t.rd); t.rd := NIL END;
      t.wr := FileWr.OpenAppend(t.path) 
    END
  END SetWriting;

PROCEDURE KeyEqual(<*UNUSED*>t : T; READONLY a, b : INTEGER) : BOOLEAN =
  (* just the default KeyEqual; may be overridden *)
  BEGIN RETURN a = b END KeyEqual;

PROCEDURE Size(t : T) : CARDINAL = BEGIN RETURN t.dir.size() END Size;

PROCEDURE WriteInt(wr : Wr.T; i : INTEGER) RAISES { Wr.Failure, Error } =
  BEGIN
    WITH t = Fmt.Int(i),
         l = Text.Length(t),
         p = Fmt.Pad(t, IndexWidth) DO
      IF l > IndexWidth THEN RAISE Error("Overflow") END;
      Wr.PutText(wr,p); Wr.Flush(wr)
    END
  END WriteInt;

PROCEDURE Write2Ints(wr : Wr.T; i,j : INTEGER) RAISES { Wr.Failure, Error } =
  BEGIN
    WITH t = Fmt.Int(i),
         l = Text.Length(t),
         p = Fmt.Pad(t, IndexWidth),

         u = Fmt.Int(j),
         m = Text.Length(u),
         q = Fmt.Pad(u, IndexWidth) DO
      IF l > IndexWidth OR m > IndexWidth THEN RAISE Error("Overflow") END;
      Wr.PutText(wr,p & q); Wr.Flush(wr)
    END
  END Write2Ints;

PROCEDURE Write3Ints(wr : Wr.T; i, j, k : INTEGER) RAISES { Wr.Failure, Error } =
  BEGIN
    WITH t = Fmt.Int(i),
         l = Text.Length(t),
         p = Fmt.Pad(t, IndexWidth),

         u = Fmt.Int(j),
         m = Text.Length(u),
         q = Fmt.Pad(u, IndexWidth),

         v = Fmt.Int(k),
         n = Text.Length(v),
         r = Fmt.Pad(v, IndexWidth) DO
      IF l > IndexWidth OR m > IndexWidth OR n > IndexWidth THEN 
        RAISE Error("Overflow")
      END;
      Wr.PutText(wr,p & q & r); Wr.Flush(wr)
    END
  END Write3Ints;

PROCEDURE ReadInt(rd : Rd.T) : INTEGER RAISES { Rd.Failure, Error } =
  VAR
    buff : ARRAY [0..IndexWidth-1] OF CHAR;
    res := 0;
    sig := 1;
  BEGIN
    IF Rd.GetSub(rd, buff) < IndexWidth THEN RAISE Error("Short read") END;
    
    FOR i := LAST(buff) TO FIRST(buff) BY -1 DO
      WITH c = buff[i] DO
        IF    c = ' ' THEN 
          EXIT 
        ELSIF c = '-' THEN
          res := -res; EXIT
        ELSE
          IF c < '0' OR c > '9' THEN RAISE Error("Unexpected character") END;
          res := res + (ORD(c) - ORD('0'))*sig
        END;
        sig := sig * 10
      END
    END;
    RETURN res
  END ReadInt;

TYPE 
  Directory = Int_IntTripleTbl.Default OBJECT 
    t : T; 
  OVERRIDES
    keyEqual := DirKeyEqual;
  END;

PROCEDURE DirKeyEqual(dir : Directory; READONLY a, b : INTEGER) : BOOLEAN =
  (* indirection thru this method permits overriding keyEqual in a T and
     having that propagate to the Directory, which is the entity whose
     keyEqual is used for comparison purposes *)
  BEGIN RETURN dir.t.keyEqual(a,b) END DirKeyEqual;

PROCEDURE Init(t : T; 
               path : Pathname.T; 
               mode : Mode; 
               initSize : [1..LAST(CARDINAL)]) : T 
  RAISES { OSError.E, Rd.Failure, Wr.Failure, Error } =
  BEGIN
    t.path := path;
    t.dir := NEW(Directory, t := t).init();
    t.freeMap := NEW(IntPQ.Default).init();
    t.dirSize := initSize;

    (* N.B. init can be called at any time, therefore we have to close
       rd and wr first, if they are open *)
    
    IF t.rd # NIL THEN Rd.Close(t.rd); t.rd := NIL END;
    IF t.wr # NIL THEN Wr.Close(t.wr); t.wr := NIL END;

    IF mode = Mode.ExistingOrCreate THEN
      TRY
        t.rd := FileRd.Open(path);
        Rd.Close(t.rd); t.rd := NIL;

        t.wr := FileWr.OpenAppend(path);
        Wr.Close(t.wr); t.wr := NIL;

        mode := Mode.ExistingOnly;

      EXCEPT
        OSError.E => mode := Mode.Replace
      END
    END;

    IF mode = Mode.Replace THEN
      t.writing();

      Wr.Seek(t.wr, 0);
      (* write header and empty directory *)
      WriteInt(t.wr, IndexWidth);
      WriteInt(t.wr, t.dirSize);
      
      FOR i := 0 TO t.dirSize-1 DO
        WriteInt(t.wr, 0); WriteInt(t.wr, 0); WriteInt(t.wr, 0);

         t.freeMap.insert(NEW(IntPQ.Elt, priority := i))
      END
    ELSE (* ExistingOnly *)
      t.reading();

      WITH iw = ReadInt(t.rd) DO
        IF iw # IndexWidth THEN 
          RAISE Error("IndexWidth in compiled ObjectStore doesn't match that in archive \"" & path & "\"") 
        END
      END;
      t.dirSize := ReadInt(t.rd);

      FOR i := 0 TO t.dirSize-1 DO
        WITH key    = ReadInt(t.rd),
             start  = ReadInt(t.rd),
             length = ReadInt(t.rd) DO
             
          IF start = 0 THEN
            (* no entry here *)
            t.freeMap.insert(NEW(IntPQ.Elt, priority := i))
          ELSE
            WITH x = t.dir.put(key, IntTriple.T { i, start, length }) DO
              IF x THEN RAISE Error("Duplicate key in archive directory") END
            END
          END
        END
      END
    END;

    RETURN t
  END Init;

PROCEDURE WriteData(wr : Wr.T; data : Elem.T) : CARDINAL
  RAISES { Wr.Failure, Pickle.Error } =
  BEGIN
    WITH start = Wr.Index(wr) DO
      Pickle.Write(wr,data); Wr.Flush(wr);
      RETURN Wr.Index(wr)-start
    END
  END WriteData;

PROCEDURE GetE(t : T; READONLY key : INTEGER; VAR value : Elem.T) : BOOLEAN
  RAISES { Rd.Failure, Pickle.Error, Rd.EndOfFile, Wr.Failure, OSError.E } = 
  VAR
    triple : IntTriple.T;
  BEGIN
    WITH res = t.dir.get(key,triple) DO
      IF res THEN 
        t.reading();
        Rd.Seek(t.rd, triple.k2);
        value := Pickle.Read(t.rd)
      END;
      RETURN res
    END
  END GetE;

PROCEDURE Get(t : T; READONLY key : INTEGER; VAR value : Elem.T) : BOOLEAN =
  <* FATAL Rd.Failure, Pickle.Error, Rd.EndOfFile, OSError.E, Wr.Failure *>
  BEGIN RETURN t.getE(key,value) END Get;

PROCEDURE PutE(t : T; 
              READONLY key : INTEGER; READONLY value : Elem.T) : BOOLEAN 
  RAISES { Wr.Failure, Rd.Failure, Rd.EndOfFile, Pickle.Error, Error, OSError.E } =
  VAR
    triple : IntTriple.T;
    start, len : CARDINAL;
  BEGIN
    (* check to see if we already have it *)
    IF t.dir.get(key,triple) THEN
      (* we already had this key, just update the old record *)
      t.writing();
      Wr.Seek(t.wr, LAST(CARDINAL));
      start := Wr.Index(t.wr);
      len := WriteData(t.wr, value);
      
      triple.k2 := start;
      triple.k3 := len;
      (*
        skip over:
        
        header                     DirOffset
        beginning of directory     3*triple.k1*IndexWidth
        my own key                 IndexWidth 
      *)
      Wr.Seek(t.wr, DirOffset + 3 * triple.k1 * IndexWidth + IndexWidth);
      Write2Ints(t.wr, start, len);
      EVAL t.dir.put(key,triple);
      RETURN TRUE
    ELSE
      (* no entry, grab one from freeMap *)
      VAR 
        de : CARDINAL;
      BEGIN
        TRY 
          de := t.freeMap.deleteMin().priority
        EXCEPT
          IntPQ.Empty => 
          WITH os = t.size() DO
            t.resize(newSize := t.dirSize*2);
            <* ASSERT t.size() = os *>
          END;
          <*FATAL IntPQ.Empty*>
          BEGIN
            de := t.freeMap.deleteMin().priority
          END
        END;

        (* write data, note that if we resized, it goes to the NEW file *)
        t.writing();
        Wr.Seek(t.wr, LAST(CARDINAL));
        start := Wr.Index(t.wr);
        len := WriteData(t.wr, value);
        
        (*
          skip over:
          
          header                     DirOffset
          beginning of directory     3*triple.k1*IndexWidth
        *)
        Wr.Seek(t.wr, DirOffset + 3 * de * IndexWidth);
        Write3Ints(t.wr, key, start, len);
        triple.k1 := de;
        triple.k2 := start;
        triple.k3 := len;
        EVAL t.dir.put(key, triple);
        RETURN FALSE
      END
    END
  END PutE;

PROCEDURE Put(t : T; READONLY key : INTEGER; 
              READONLY value : Elem.T) : BOOLEAN =
  <* FATAL Rd.Failure, Rd.EndOfFile, Pickle.Error, Error, OSError.E, Wr.Failure *>
  BEGIN RETURN t.putE(key,value) END Put;

PROCEDURE Delete(t : T; READONLY key : INTEGER; 
                 VAR value : Elem.T) : BOOLEAN =
  <* FATAL Rd.Failure, Rd.EndOfFile, Pickle.Error, Wr.Failure, Error, OSError.E *>
  BEGIN RETURN t.deleteE(key,value) END Delete;

PROCEDURE DeleteE(t : T; READONLY key : INTEGER; VAR value : Elem.T) : BOOLEAN 
  RAISES { Rd.Failure, Rd.EndOfFile, Pickle.Error, Wr.Failure, Error, OSError.E } =
  VAR
    triple : IntTriple.T;
  BEGIN
    IF t.dir.delete(key,triple) THEN
      (*
        skip over:
        
        header                     DirOffset
        beginning of directory     3*triple.k1*IndexWidth
        my own key                 IndexWidth 
      *)
      t.writing();
      Wr.Seek(t.wr, DirOffset + 3 * triple.k1 * IndexWidth + IndexWidth);
      WriteInt(t.wr, 0);

      t.reading();
      Rd.Seek(t.rd, triple.k2);
      value := Pickle.Read(t.rd);

      (* index *)
      t.freeMap.insert(NEW(IntPQ.Elt, priority := triple.k1));

      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END DeleteE;

(**********************************************************************)

PROCEDURE Resize(t : T; newSize : CARDINAL) 
  RAISES { Wr.Failure, OSError.E, Error, Rd.Failure, Pickle.Error, Rd.EndOfFile } =
  VAR
    k : INTEGER;
    r : Elem.T;
  BEGIN
    WITH fn = t.path & "." & 
         Fmt.Int(NEW(Random.Default).init().integer(16_100000,16_ffffff), 
                 base := 16),
         dummy = NEW(T).init(fn,Mode.Replace,initSize := newSize),
         iter = t.iterateE() DO
      WHILE iter.nextE(k,r) DO
        WITH x = dummy.put(k,r) DO
          <* ASSERT NOT x *>
        END
      END;
      
      (* dummy is a faithful, but bigger, copy *)
      IF t.rd # NIL THEN Rd.Close(t.rd); t.rd := NIL END; 
      IF t.wr # NIL THEN Wr.Close(t.wr); t.wr := NIL END;

      (* this is windows baloney, can't rename an open file *)
      IF dummy.rd # NIL THEN Rd.Close(dummy.rd); dummy.rd := NIL END;
      IF dummy.wr # NIL THEN Wr.Close(dummy.wr); dummy.wr := NIL END;

      FS.Rename(fn,t.path);
      t.dir := dummy.dir;
      t.freeMap := dummy.freeMap;
      t.rd := dummy.rd;
      t.wr := dummy.wr;
      t.dirSize := dummy.dirSize
    END
  END Resize;

(**********************************************************************)

REVEAL
  Iterator = PubIterator BRANDED Brand & " Iterator" OBJECT
    it : REF ARRAY OF IntInt.T;
    i : CARDINAL;
    t : T;
  OVERRIDES
    next := INext;
    nextE := INextE;
  END;

PROCEDURE INext(iter : Iterator; VAR k : INTEGER; VAR r : Elem.T) : BOOLEAN =
  <* FATAL Rd.Failure, Pickle.Error, Rd.EndOfFile, OSError.E, Wr.Failure *>
  BEGIN RETURN iter.nextE(k,r) END INext;

PROCEDURE INextE(iter : Iterator; VAR k : INTEGER; VAR r : Elem.T) : BOOLEAN 
  RAISES { Rd.Failure, Rd.EndOfFile, Pickle.Error, OSError.E, Wr.Failure } =
  BEGIN 
    IF iter.i = NUMBER(iter.it^) THEN RETURN FALSE END;
    
    WITH rec = iter.it[iter.i],
         x = iter.t.getE(rec.value,r) DO
      <* ASSERT x *>
      k := rec.value;
      INC(iter.i);
      RETURN TRUE
    END
  END INextE;

PROCEDURE Iterate(t : T) : IntElemTbl.Iterator = 
  BEGIN RETURN t.iterateE() END Iterate;

PROCEDURE IterateE(t : T) : Iterator =
  VAR
    k : INTEGER;
    triple : IntTriple.T;
    i : CARDINAL := 0;
  BEGIN
    (* an efficient iterator, requires planning *)
    WITH indextab = NEW(REF ARRAY OF IntInt.T, t.size()),
         iter = t.dir.iterate() DO
      WHILE iter.next(k,triple) DO
        indextab[i] := IntInt.T { triple.k2, k };
        INC(i)
      END;
      IntIntArraySort.Sort(indextab^);
      RETURN NEW(Iterator, i := 0, it := indextab, t := t)
    END 
  END IterateE;

BEGIN END PersistentTable.





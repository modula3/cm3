(*---------------------------------------------------------------------------*)
MODULE FilePool;

IMPORT Rd, Wr, File, FileRd, FileWr, TextSeq, TextIntTbl, Pathname, RegEx,
       OSError, Thread, ASCII, Random, Text;
IMPORT FSUtils;
IMPORT (* FSFixed AS *) FS;

(*---------------------------------------------------------------------------*)
REVEAL
  T = Public BRANDED "FilePool 0.0"  OBJECT
    dir  : TextIntTbl.T;
    path : TEXT;
  METHODS
    newDir(dir : Pathname.T) : TextIntTbl.T RAISES {Error} := NewDir;
  OVERRIDES
    init := Init;
    update := Update;
    list := List;
    getReader := GetReader;
    getWriter := GetWriter;
    content := Content;
    createNewFile := CreateNewFile;
    delete := Delete;
    apply := Apply;
    select := Select;
  END;

(*---------------------------------------------------------------------------*)
CONST
  AttrFile = 1;
  AttrDir  = 2;
  
(*---------------------------------------------------------------------------*)
PROCEDURE NewDir(<* UNUSED *> self : T; dir : Pathname.T) : TextIntTbl.T 
  RAISES {Error} =
  VAR
    res := NEW(TextIntTbl.Default).init();
  BEGIN
    IF NOT FSUtils.IsDir(dir) THEN
      RAISE Error("no directory " & dir);
    END;
    TRY
      VAR 
	it := FS.Iterate(dir); 
	fn  : TEXT;
        attr : INTEGER;
      BEGIN
	TRY
	  TRY <* NOWARN *> (* no exceptions currently, but... *)
	    WHILE it.next(fn) DO
              IF FSUtils.IsFile(fn) THEN
                attr := AttrFile;
              ELSIF FSUtils.IsDir(fn) THEN
                attr := AttrDir;
              ELSE
                attr := 0;
              END;
              EVAL res.put(fn, attr);
	    END;
	  EXCEPT ELSE
	    RAISE Error("error reading directory " & dir);
	  END;
	FINALLY
	  it.close();
	END;
      END;
    EXCEPT
      OSError.E => RAISE Error("cannot open directory " & dir);
    END;
    RETURN res;
  END NewDir;

(*---------------------------------------------------------------------------*)
PROCEDURE Init(self : T; dir : Pathname.T) : T RAISES {Error} =
  BEGIN
    self.path := dir;
    self.dir := self.newDir(dir);
    RETURN self;
  END Init;

(*---------------------------------------------------------------------------*)
PROCEDURE Update(self : T) RAISES {Error} =
  BEGIN
    self.dir := self.newDir(self.path);
  END Update;

(*---------------------------------------------------------------------------*)
PROCEDURE List(self : T; pattern : TEXT := NIL; ordinaryOnly := TRUE) 
  : TextSeq.T RAISES {Error} =
  VAR
    it   := self.dir.iterate();
    fn   :  TEXT;
    attr :  INTEGER;
    res  := NEW(TextSeq.T).init();
  BEGIN
    WHILE it.next(fn, attr) DO
      IF NOT ordinaryOnly OR attr = AttrFile THEN
        IF pattern = NIL OR Matches(pattern, fn) THEN
          res.addhi(fn);
        END;
      END;
    END;
    RETURN res;
  END List;

(*---------------------------------------------------------------------------*)
PROCEDURE Matches(pattern, text : TEXT) : BOOLEAN RAISES {Error} =
  VAR res := FALSE;
  BEGIN
    TRY
      VAR
        pat := RegEx.Compile(pattern);
      BEGIN
        res := RegEx.Execute(pat, text) > -1;
      END;
    EXCEPT
      RegEx.Error => RAISE Error("invalid regular expressions: " & pattern);
    END;
    RETURN res;
  END Matches;

(*---------------------------------------------------------------------------*)
PROCEDURE GetReader(self : T; fn : TEXT) : Rd.T RAISES {Error} =
  VAR 
    attr : INTEGER;
    rd   : FileRd.T;
    path : Pathname.T;
  BEGIN
    IF NOT self.dir.get(fn, attr) THEN
      RAISE Error("not found: " & fn);
    END;
    TRY
      path := Pathname.Join(self.path, fn, NIL);
      rd := FileRd.Open(path);
    EXCEPT
      OSError.E => RAISE Error("cannot open file for reading: " & path);
    END;
    RETURN rd;
  END GetReader;

(*---------------------------------------------------------------------------*)
PROCEDURE GetWriter(self : T; fn : TEXT) : Wr.T RAISES {Error} =
  VAR 
    wr   : FileWr.T;
    path : Pathname.T;
  BEGIN
    TRY
      path := Pathname.Join(self.path, fn, NIL);
      wr := FileWr.Open(path);
    EXCEPT
      OSError.E => RAISE Error("cannot open file for writing: " & path);
    END;
    RETURN wr;
  END GetWriter;

(*---------------------------------------------------------------------------*)
PROCEDURE Content(self : T; fn : TEXT) : TEXT RAISES {Error} =
  VAR
    rd  := self.getReader(fn);
    res :  TEXT;
  BEGIN
    TRY
      res := Rd.GetText(rd, LAST(CARDINAL));
    EXCEPT
      Rd.Failure => RAISE Error("cannot read file " & fn);
    | Thread.Alerted => RAISE Error("interrupted reading file " & fn);
    END;
    RETURN res;
  END Content;

(*---------------------------------------------------------------------------*)
PROCEDURE CreateNewFile(self : T; fn : TEXT := NIL) : TEXT RAISES {Error} =
  VAR
    attr : INTEGER;
    okay : BOOLEAN := FALSE;
    file : File.T;

  (*-------------------------------------------------------------------------*)
  PROCEDURE CreateFile(fn : TEXT) : File.T RAISES {Error} =
    VAR
      f    : File.T;
      path : Pathname.T;
    BEGIN
      path := Pathname.Join(self.path, fn, NIL);
      TRY
        f := FS.OpenFile(path, truncate := TRUE, 
                         create := FS.CreateOption.Always);
      EXCEPT ELSE
        RAISE Error("cannot create file " & path);
      END;
      RETURN f;
    END CreateFile;

  (*-------------------------------------------------------------------------*)
  BEGIN (* CreateNewFile *)
    IF fn = NIL THEN
      WHILE NOT okay DO
        okay := TRUE;
        fn := FileName();
        TRY
          file := CreateFile(fn);
        EXCEPT ELSE
          okay := FALSE;
        END;
        IF okay THEN
          EVAL self.dir.put(fn, AttrFile);
        END;
      END;
    ELSE
      IF self.dir.get(fn, attr) THEN
        RAISE Error("already in cache: " & fn);
      END;
      file := CreateFile(fn);
      EVAL self.dir.put(fn, AttrFile);
    END;
    TRY file.close(); EXCEPT ELSE END;
    RETURN fn;
  END CreateNewFile;

(*---------------------------------------------------------------------------*)
PROCEDURE FileName() : TEXT =
  VAR
    m   : INTEGER;
    c   : CHAR;
    res : TEXT := "";
  BEGIN
    FOR i := 1 TO 8 DO
      REPEAT
        m := random.integer(97, 122);
      UNTIL VAL(m, ASCII.Range) IN ASCII.Letters;
      c := VAL(m, CHAR);
      res := res & Text.FromChar(c);
    END;
    RETURN res;
  END FileName;

(*---------------------------------------------------------------------------*)
PROCEDURE Delete(self : T; fn : TEXT) RAISES {Error} =
  VAR 
    attr : INTEGER;
    path : Pathname.T;
  BEGIN
    IF NOT self.dir.get(fn, attr) THEN
      RAISE Error("not found: " & fn);
    END;
    TRY
      path := Pathname.Join(self.path, fn, NIL);
      FS.DeleteFile(path);
    EXCEPT
      OSError.E => RAISE Error("cannot delet file " & path);
    END;
    EVAL self.dir.delete(fn, attr);
  END Delete;

(*---------------------------------------------------------------------------*)
PROCEDURE Apply(self : T; cl : ProcClosure; 
                pattern : TEXT := NIL; ordinaryOnly := TRUE) RAISES ANY =
  VAR
    list := self.list(pattern, ordinaryOnly);
  BEGIN
    FOR i := 0 TO list.size() - 1 DO
      WITH fn = list.get(i) DO
        cl.proc(fn);
      END;
    END;
  END Apply;

(*---------------------------------------------------------------------------*)
PROCEDURE Select(self : T; cl : PredClosure;
                 pattern : TEXT := NIL; ordinaryOnly := TRUE) : TextSeq.T
  RAISES {Error} =
  VAR
    list := self.list(pattern, ordinaryOnly);
    res  := NEW(TextSeq.T).init();
  BEGIN
    FOR i := 0 TO list.size() - 1 DO
      WITH fn = list.get(i) DO
        IF cl.pred(fn) THEN
          res.addhi(fn);
        END;
      END;
    END;
    RETURN res;
  END Select;

(*---------------------------------------------------------------------------*)
VAR
  random := NEW(Random.Default).init();
BEGIN
END FilePool.

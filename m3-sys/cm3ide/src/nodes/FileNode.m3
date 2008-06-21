(* Copyright 1996 Critical Mass, Inc. All rights reserved.    *)

MODULE FileNode;

IMPORT Pathname, Rd, Thread, Wr;
IMPORT ConfigItem, ErrLog, HTML, ID, Node, OS, Text2, Wx;

TYPE
  FileKind = [0..LAST(FileMap)];
  ExtMap   = RECORD extension, file_type: TEXT; END;

CONST
  FirstHTML = 1;  (* index into FileMap *)
  LastHTML  = 2;

CONST
  FileMap = ARRAY OF ExtMap {
    ExtMap { "???",     "text/plain" },  (* initial kind = 0 *)
    ExtMap { "html",    "text/html" },
    ExtMap { "htm",     "text/html" },
    ExtMap { "gif",     "image/gif" },
    ExtMap { "ps",      "application/postscript" },
    ExtMap { "pdf",     "application/pdf" },
    ExtMap { "text",    "text/plain" },
    ExtMap { "txt",     "text/plain" },
    ExtMap { "jpeg",    "image/jpeg" },
    ExtMap { "jpg",     "image/jpeg" },
    ExtMap { "jpe",     "image/jpeg" },
    ExtMap { "gz",      "application/x-gzip" },
    ExtMap { "Z",       "application/x-compress" },
    ExtMap { "js",      "application/x-javascript" },
    ExtMap { "ls",      "application/x-javascript" },
    ExtMap { "mocha",   "application/x-javascript" },
    ExtMap { "tcl",     "application/x-tcl" },
    ExtMap { "sh",      "application/x-sh" },
    ExtMap { "csh",     "application/x-csh" },
    ExtMap { "ai",      "application/postscript" },
    ExtMap { "eps",     "application/postscript" },
    ExtMap { "exe",     "application/octet-stream" },
    ExtMap { "bin",     "application/octet-stream" },
    ExtMap { "gtar",    "application/x-gtar" },
    ExtMap { "tar",     "application/x-tar" },
    ExtMap { "zip",     "application/x-zip-compressed" },
    ExtMap { "sit",     "application/x-stuffit" },
    ExtMap { "shar",    "application/x-shar" },
    ExtMap { "hqx",     "application/mac-binhex40" },
    ExtMap { "rtf",     "application/rtf" },
    ExtMap { "tex",     "application/x-tex" },
    ExtMap { "dvi",     "application/x-dvi" },
    ExtMap { "latex",   "application/x-latex" },
    ExtMap { "texi",    "application/x-texinfo" },
    ExtMap { "texinfo", "application/x-texinfo" },
    ExtMap { "avi",     "video/x-msvideo" },
    ExtMap { "qt",      "video/quicktime" },
    ExtMap { "mov",     "video/quicktime" },
    ExtMap { "mpeg",    "video/mpeg" },
    ExtMap { "mpg",     "video/mpeg" },
    ExtMap { "mpe",     "video/mpeg" },
    ExtMap { "wav",     "audio/x-wav" },
    ExtMap { "aif",     "audio/x-aiff" },
    ExtMap { "aiff",    "audio/x-aiff" },
    ExtMap { "aifc",    "audio/x-aiff" },
    ExtMap { "au",      "audio/basic" },
    ExtMap { "snd",     "audio/basic" },
    ExtMap { "bmp",     "image/x-MS-bmp" },
    ExtMap { "rgb",     "image/x-rgb" },
    ExtMap { "ppm",     "image/x-portable-pixmap" },
    ExtMap { "pgm",     "image/x-portable-graymap" },
    ExtMap { "pbm",     "image/x-portable-bitmap" },
    ExtMap { "pnm",     "image/x-portable-anymap" },
    ExtMap { "wxd",     "image/x-xwindowdump" },
    ExtMap { "xpm",     "image/x-xpixmap" },
    ExtMap { "xbm",     "image/x-xbitmap" },
    ExtMap { "ras",     "image/x-cmu-raster" },
    ExtMap { "ief",     "image/ief" },
    ExtMap { "tiff",    "image/tiff" },
    ExtMap { "tif",     "image/tiff" },
    ExtMap { "",        "text/plain" }   (* default kind = text *)
  };

REVEAL
  T = Tx BRANDED "FileNode.T" OBJECT
    kind : FileKind := 0;
  OVERRIDES
    class    := Class;
    iterate  := Iterate;
    next     := Next;
    gen_page := GenPage;
  END;

PROCEDURE Class (<*UNUSED*> t: T): Node.Class =
  BEGIN
    RETURN Node.Class.RawFile;
  END Class;

PROCEDURE Iterate (<*UNUSED*> t: T;  <*UNUSED*> VAR s: Node.IteratorState) =
  BEGIN
  END Iterate;

PROCEDURE Next (<*UNUSED*> t: T;  <*UNUSED*> VAR s: Node.IteratorState): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Next;

PROCEDURE GenPage (t: T;  wx: Wx.T;
                   <*UNUSED*> action: ID.T;
                   <*UNUSED*> data: Node.FormData)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (t.kind = 0) THEN SetKind (t); END;
    wx.put ("Content-type: ", FileMap[t.kind].file_type, "\n");
    (** HTML.GenLocation (t, wx); **)
    IF ConfigItem.X [ConfigItem.T.Use_multiple_windows].bool THEN
      wx.put ("Window-target: ", Node.ClassWindow [Node.Class.RawFile], "\n");
    END;
    wx.put ("\n"); (* end of HTTP header *)
    CopyFile (t, t.path, wx, t.kind);
  END GenPage;

PROCEDURE EmitFile (n: Node.T;  path: TEXT;  wx: Wx.T): BOOLEAN
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR kind := FindKind (path);
  BEGIN
    IF (kind = LAST (FileKind)) THEN (*unknown type*) RETURN FALSE; END;
    wx.put ("Content-type: ", FileMap[kind].file_type, "\n");
    IF ConfigItem.X [ConfigItem.T.Use_multiple_windows].bool THEN
      wx.put ("Window-target: ", Node.ClassWindow [Node.Class.RawFile], "\n");
    END;
    wx.put ("\n"); (* end of HTTP header *)
    CopyFile (n, path, wx, kind);
    RETURN TRUE;
  END EmitFile;

PROCEDURE SetKind (t: T) =
  BEGIN
    t.kind := FindKind (ID.ToText (t.name));
  END SetKind;

PROCEDURE FindKind (nm: TEXT): FileKind =
  VAR ext := Pathname.LastExt (nm);
  BEGIN
    FOR k := 1 TO LAST (FileKind) DO
      IF OS.FileNameEq (ext, FileMap[k].extension) THEN RETURN k; END;
    END;
    RETURN LAST (FileKind);
  END FindKind;

PROCEDURE CopyFile (n: Node.T;  path: TEXT;  wx: Wx.T;  kind: FileKind)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* This procedure modified by R.Coleburn on 2008_0620 to add copyright legend to static HTML pages
  in order to comply with open-source release licensing requirements specified by Farshad Nayeri. *)
  VAR
    rd  := OS.OpenRd (path);
    len : INTEGER;
    buf : ARRAY [0..1023] OF CHAR;
    first_time := TRUE;
    fileIsHTML := (FirstHTML <= kind) AND (kind <= LastHTML);
    start, i : CARDINAL;
    endBody_UC := ARRAY [0..5] OF CHAR {'<', '/', 'B', 'O', 'D', 'Y'};
    endBody_LC := ARRAY [0..5] OF CHAR {'<', '/', 'b', 'o', 'd', 'y'};
    state: CARDINAL := 0;
    found: BOOLEAN := FALSE;
  BEGIN
    IF (rd = NIL) THEN RETURN END;
    (*DebugMsg("!!!RCC::FileNode.CopyFile; name=" & n.printname());*)
    TRY
      TRY
        start := 0;
        LOOP
          len := Rd.GetSub (rd, SUBARRAY(buf, start, (NUMBER(buf) - start))) + start;
          (*DebugMsg("FileNode.CopyFile; read " & Fmt.Int(len - start) & " chars into buf");*)
          start := 0;
          IF (len <= 0) THEN EXIT END;
          IF (first_time) AND fileIsHTML THEN
            start := FixHTML (n, SUBARRAY (buf, start, len), wx);
            DEC(len, start);
            IF (len <= 0) THEN EXIT END;
            (*DebugMsg("FileNode.CopyFile; after FixHTML, start=" & Fmt.Int(start));*)
            first_time := FALSE;
          END;
          
          IF (NOT found) AND fileIsHTML
          THEN (* add copyright legend before "/body" to comply with Farshad's licensing requirement *)
             i := start;
             LOOP
                WITH c = buf[i]
                DO
                  IF (c = endBody_UC[state]) OR (c = endBody_LC[state])
                  THEN (* match so far *)
                     INC(state);
                     IF state >= NUMBER(endBody_UC)
                     THEN (* a complete match *)
                        found := TRUE;
                        (*DebugMsg("FileNode.CopyFile; found, start=" & Fmt.Int(start) & ", i=" & Fmt.Int(i) & ", len=" & Fmt.Int(len) & ", state=" & Fmt.Int(state));*)
                        
                        (* write the part of buffer preceeding the match *)
                        WITH num = i - start - NUMBER(endBody_UC) + 1
                        DO 
                           wx.putStr (SUBARRAY (buf, start, num));
                           DEC(len, num);
                           INC(start, num);
                           (*DebugMsg("FileNode.CopyFile; wrote " & Fmt.Int(num) & " chars from buf, start=" & Fmt.Int(start) & ", i=" & Fmt.Int(i) & ", len=" & Fmt.Int(len));*)
                        END; (* with *)

                        (* write the copyright *)
                        HTML.GenCopyright(wx);
                        (*DebugMsg("FileNode.CopyFile; added copyright for " & n.printname());*)
                        
                        (* write the end-body tag and the rest of the buffer *)
                        IF len > 0
                        THEN
                           wx.putStr (SUBARRAY (buf, start, len));
                           (*DebugMsg("FileNode.CopyFile; emptied " & Fmt.Int(len) & " chars from buf");*)
                        END; (* if *)

                        (* done with this buffer *)
                        start := 0;
                        state := 0;
                        EXIT;
                     END; (* if *)
                  ELSE (* no match *)
                     state := 0;
                  END; (* if *)
                END; (* with *)
                INC(i);
                IF i >= len
                THEN (* exhausted this buffer, so write it out and prepare to read next buffer *)
                   (*DebugMsg("FileNode.CopyFile; buffer exhausted, start=" & Fmt.Int(start) & ", i=" & Fmt.Int(i) & ", len=" & Fmt.Int(len) & ", state=" & Fmt.Int(state));*)
                   wx.putStr (SUBARRAY (buf, start, (len - state)));
                   IF state > 0
                   THEN (* we've seen part of the end-tag, so put this part back in the buffer for the next round *)
                      SUBARRAY (buf, 0, state) := SUBARRAY(endBody_UC, 0, state);
                      start := state;
                   ELSE
                      start := 0;
                   END; (* if *)
                   EXIT;
                END; (* if *)
             END; (* loop *)
          ELSE
             wx.putStr (SUBARRAY (buf, start, len));
          END;
        END;
      FINALLY
        OS.CloseRd (rd);
      END;
    EXCEPT Rd.Failure (ec) =>
      ErrLog.Msg ("Read failed on \"", path, "\"", OS.Err (ec));
    END;
  END CopyFile;

CONST
  HeadMarks  = ARRAY OF TEXT { "</HEAD>", "</head>", "<BODY>", "<body>" };
  TitleMarks = ARRAY OF TEXT { "</H1>","</h1>","</H2>","</h2>","</H3>","</h3>" };
  BaseMarks  = ARRAY OF TEXT { "<BASE", "<base" };

PROCEDURE FixHTML (n: Node.T;  READONLY buf: ARRAY OF CHAR;  wx: Wx.T): INTEGER
  RAISES {Wr.Failure, Thread.Alerted} =
  (* This procedure modified by R.Coleburn on 2008_0620 to return the 
  index position of the start of the buffer that has not been processed yet,
  rather than going ahead and writing this unprocessed part of the buffer. *)
  VAR base_loc, end_head, end_title, done: INTEGER;
  BEGIN
    IF (NUMBER (buf) <= 0) THEN RETURN 0 END;

    end_head  := FindMark (buf, HeadMarks);
    base_loc  := FindMark (buf, BaseMarks);
    end_title := FindMark (buf, TitleMarks);

    done := 0;
    IF (n # NIL) AND (base_loc < 0) AND (end_head >= 0) THEN
      (* the file doesn't have a <BASE> tag and we know where to put one! *)
      wx.putStr (SUBARRAY (buf, 0, end_head));   done := end_head;
      HTML.GenBase (n, wx, leaf := TRUE);
    END;

    IF (n # NIL) AND (end_title > done) THEN
      (* we found a title, let's add a pathfinder after it *)
      wx.putStr (SUBARRAY (buf, done, end_title + 5 - done));
      done := end_title + 5;
      HTML.GenPathFinder (n, wx);
    END;

    RETURN done;
  END FixHTML;

PROCEDURE FindMark (READONLY buf   : ARRAY OF CHAR;
                    READONLY marks : ARRAY OF TEXT): INTEGER =
  VAR x: INTEGER;
  BEGIN
    FOR i := FIRST (marks) TO LAST (marks) DO
      x := Text2.FindBufSubstring (buf, marks[i]);
      IF (x >= 0) THEN RETURN x; END;
    END;
    RETURN -1;
  END FindMark;

BEGIN
END FileNode.

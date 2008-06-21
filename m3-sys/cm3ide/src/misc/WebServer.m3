(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

MODULE WebServer;

IMPORT Fmt, IP, Pathname, RTCollectorSRC, Text, TextList, Time, Wr, Thread;
IMPORT Default, ConfigItem, BrowserDB, Buf, ErrLog, HTML, ID;
IMPORT Node, RegExpr, TCPServer, Text2, Wx;
IMPORT TextRd, TextWr, UnsafeRd, Rd;

TYPE
  CI = ConfigItem.T;

CONST
  BackSlash = '\134';

TYPE
  Binding = RECORD
    tag  : TEXT   := NIL;
    root : Node.T := NIL;
  END;

CONST
  NoBinding = Binding { NIL, NIL };

VAR
  n_bindings : INTEGER := 0;
  bindings   : ARRAY [0..127] OF Binding;
  default    : Binding;
  mu         := NEW (MUTEX);
  IsBlank    : ARRAY CHAR OF BOOLEAN;
  viewID     : ID.T;
  time_now   := Time.Now ();
  server     : TCPServer.T := NIL;
  service    : ServiceClosure := NIL;
  changed    := NEW (Thread.Condition);

EXCEPTION
  Error (TEXT);



(* --------------------------------------------------------------------------------------------- *)
(* The following code was swiped from module HTTP and adapted for use here.
 * Note: At some time it may make sense to recode this module using the HTTP interface, 
 * but for now, the UnescapeURLEntry proc is needed for a patch that Olaf Wagner suggested
 * and I didn't want to import both HTTP & App without fully understanding what issues might
 * result from these imports. --RCC
 *)

PROCEDURE HexToInt(ch: CHAR): INTEGER RAISES {Error} =
  BEGIN
    IF ('0' <= ch AND ch <= '9') THEN
      RETURN ORD(ch) - ORD('0');
    ELSIF ('A' <= ch AND ch <= 'F') THEN
      RETURN ORD(ch) - ORD('A') + 10;
    ELSIF ('a' <= ch AND ch <= 'f') THEN
      RETURN ORD(ch) - ORD('a') + 10;
    ELSE
      RAISE Error(NIL);
    END;
  END HexToInt;

PROCEDURE UnescapeURLEntry(body: TEXT): TEXT RAISES {Error} =
  VAR
    trd := TextRd.New(body);
    twr := TextWr.New();
    ch: CHAR;
    <* FATAL Wr.Failure, Thread.Alerted *> (* not sure I like that these are marked fatal --RCC *)
  BEGIN
    TRY
      WHILE NOT UnsafeRd.FastEOF(trd) DO
        ch := UnsafeRd.FastGetChar(trd);
        IF ch = '%' THEN
          Wr.PutChar(twr, VAL(HexToInt(UnsafeRd.FastGetChar(trd)) * 16 + 
                              HexToInt(UnsafeRd.FastGetChar(trd)), CHAR));
        ELSIF ch = '+' THEN
          Wr.PutChar(twr, ' ');
        ELSE
          Wr.PutChar(twr, ch);
        END;
      END;
    EXCEPT
    | Error, Rd.Failure, Rd.EndOfFile =>
        RAISE Error(Fmt.F("Badly escaped URL body: %s", body));
    END;
    RETURN TextWr.ToText(twr);
  END UnescapeURLEntry;

(* ---- end swiped code from HTTP ----------------------------------------------- *)



(*------------------------------------------------------ main run loop ---*)

PROCEDURE Run () =
  BEGIN
    EVAL Thread.Fork (NEW (Thread.Closure, apply := Cleaner));
    Restart ();
    LOCK mu DO
      WHILE (server # NIL) DO Thread.Wait (mu, changed); END;
    END;
  END Run;

PROCEDURE Restart () =
  VAR cl := NEW (ServiceClosure);
  BEGIN
    LOCK mu DO
      IF (server # NIL) THEN
        service.abort := TRUE;
        TCPServer.Abort (server);
        WHILE (server # NIL) DO Thread.Wait (mu, changed); END;
      END;
      cl.ip_addr   := ConfigItem.X [CI.IP_address].addr;
      cl.port      := ConfigItem.X [CI.Server_port].int;
      cl.n_workers := ConfigItem.X [CI.Num_server_threads].int;
      cl.refresh   := ConfigItem.X [CI.Refresh_interval].int;
      cl.started   := FALSE;
      cl.abort     := FALSE;
      service := cl;
      EVAL Thread.Fork (cl);
      WHILE NOT cl.started DO Thread.Wait (mu, changed); END;
    END;
  END Restart;

TYPE
  ServiceClosure = Thread.Closure OBJECT
    ip_addr   : IP.Address;
    port      : INTEGER;
    n_workers : INTEGER;
    refresh   : INTEGER;
    started   : BOOLEAN;
    abort     : BOOLEAN;
  OVERRIDES
    apply := RunService;
  END;

PROCEDURE RunService (cl: ServiceClosure): REFANY =
  VAR me: TCPServer.T;
  BEGIN
    WHILE NOT cl.abort DO
      ErrLog.Msg ("starting TCP service");
      me := TCPServer.Fork (cl.ip_addr, cl.port, cl.n_workers, ProcessRequest,
                  Refresh, cl.refresh, ErrLog.Note);
      server := me;
      cl.started := TRUE;
      Thread.Broadcast (changed);
      IF (me = NIL) THEN
        ErrLog.Msg ("unable to start TCP service");
        EXIT;
      END;
      TCPServer.Join (server);
      ErrLog.Msg ("TCP service stopped.");
    END;
    ErrLog.Msg ("TCP service closed.");
    server := NIL;
    Thread.Broadcast (changed);
    RETURN NIL;
  END RunService;

PROCEDURE Refresh (<*UNUSED*> server: TCPServer.T) =
  BEGIN
    TRY
      BrowserDB.Refresh (NIL);
      time_now := Time.Now ();
    EXCEPT Thread.Alerted =>
      (* IGNORE *)
    END;
  END Refresh;

PROCEDURE Cleaner (<*UNUSED*> self: Thread.Closure): REFANY =
  (* Try to keep the heap as clean as possible... *)
  BEGIN
    RTCollectorSRC.StartBackgroundCollection ();
(***
    LOOP
      RTCollectorSRC.StartCollection ();
      Thread.Pause (30.0d0);
      RTCollectorSRC.FinishCollection ();
    END;
***)
RETURN NIL;
  END Cleaner;

(*----------------------------------------------- handler registration ---*)

VAR next_fake := 0;

PROCEDURE RegisterRoot (tag: TEXT;  root: Node.T) =
  VAR new_tag: TEXT;
  BEGIN
    LOCK mu DO
      WHILE NOT TryRegister (tag, root) DO
        new_tag := "Root-" & Fmt.Int (next_fake);  INC (next_fake);
        Duplicate (tag, new_tag);
        tag := new_tag;
      END;
    END;
  END RegisterRoot;

PROCEDURE UnregisterRoot (tag: TEXT) =
  VAR ok := FALSE;
  BEGIN
    LOCK mu DO
      IF tag = NIL THEN
        default := NoBinding;
        tag := "<NIL>";
        ok := TRUE;
      ELSE
        FOR i := 0 TO n_bindings-1 DO
          IF Text.Equal (tag, bindings[i].tag) THEN
            ok := TRUE;
            FOR j := i+1 TO n_bindings-1 DO
              bindings[j-1] := bindings[j];
            END;
            bindings [n_bindings-1] := NoBinding;
            DEC (n_bindings);
            EXIT;
          END;
        END;
      END;
    END;
    IF ok AND ConfigItem.X [CI.Verbose_log].bool THEN
      ErrLog.Msg ("/", tag, " unregistered.");
    END;
  END UnregisterRoot;

PROCEDURE TryRegister (tag: TEXT;  root: Node.T): BOOLEAN =
  (* LL = mu *)
  VAR i := 0;
  BEGIN
    IF (tag = NIL) THEN
      IF (default.root # NIL) THEN RETURN FALSE; END;
      default.root := root;
      tag := "<NIL>";  (* for the log message below *)
    ELSE
      WITH b = bindings[n_bindings] DO  b.tag  := tag; b.root := root;  END;
      WHILE NOT Text.Equal (bindings[i].tag, tag) DO INC(i); END;
      IF (i < n_bindings) THEN RETURN FALSE; END;
      INC (n_bindings);
    END;
    IF ConfigItem.X [CI.Verbose_log].bool THEN
      ErrLog.Msg ("/", tag, " registered.");
    END;
    RETURN TRUE;
  END TryRegister;

PROCEDURE Duplicate (old, new: TEXT) =
  BEGIN
    IF (old = NIL) THEN old := "<NIL>"; END;
    ErrLog.Msg ("Attempted to register duplicate root \"", old,
                "\", using \"", new & "\" instead." );
  END Duplicate;

(*----------------------------------------------- main request server ---*)

TYPE
  URL = RECORD
    action : ID.T;
    data   : Node.FormData;
    n_arcs : INTEGER;
    arcs   : ARRAY [0..49] OF Arc;
    query_merge : BOOLEAN;
  END;

TYPE
  Arc = RECORD
    pattern : TEXT;
    expr    : RegExpr.T;
    count   : INTEGER := 0;
    hit     : Node.T  := NIL;
  END;

PROCEDURE ProcessRequest (cmd: TEXT;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST
    Header_OK    = "HTTP/1.0 200 ok\r\n";
    Header_Moved = "HTTP/1.0 301 moved\r\n";
 (* Header_Nope  = "HTTP/1.0 204 no content\r\n"; *)

  VAR
    results : Node.Set;
    url     : URL;
    node    : Node.T;
    url_txt : TEXT;
    url_msg : TEXT;
  BEGIN
    IF ConfigItem.X [CI.Verbose_log].bool THEN
      ErrLog.Msg (Clean (cmd));
    END;
    TRY
      ParseRequest (cmd, url);
    EXCEPT Error (msg) =>
      ErrLog.Msg ("Bad request: \"", Clean (cmd), "\": ", msg);
      wx.put ("HTTP/1.0 400 unknown request: \"", Clean (cmd),
              "\": " & msg & "\r\n");
      RETURN;
    END;

    CollectResults (url, results);
    Node.Squash (results);

    CASE results.cnt OF
    | 0 =>  wx.put (Header_OK);
            (** GenLocation (url, wx); **)
            HTML.BeginXX (NIL, wx, "No matching results", icon := "what");
            NoResults (url, wx);
            HTML.End (wx);

    | 1 =>  node := results.elts[0];
            IF NOT MatchingURL (node, url) THEN
              url_txt := HTML.NodeURL (node);
              IF (url.action # viewID) AND (url.action # ID.NoID) THEN
                url_txt := url_txt & "[" & ID.ToText (url.action) & "]";
              END;
              url_msg := url_txt;
              IF Text2.FindSubstring (url_msg, Default.server_href) = 0 THEN
                url_msg := Text.Sub (url_msg, Text.Length (Default.server_href)-1);
              END;
              ErrLog.Msg ("redirect ", URLtoText (url), " --> ", url_msg);
              wx.put (Header_Moved);
              wx.put ("Location: ", url_txt, "\n");
            ELSE
              wx.put (Header_OK);
            END;
            node.gen_page (wx, url.action, url.data);

    ELSE    (** wx.put (Header [TRUE]); **)
            (* --- this next line seems to be a bug, replaced with wx.put of Header_OK; RCC, 2008_0122
               wx.put (Header_Moved);
            *)
            wx.put (Header_OK);
            GenLocation (url, wx);
            url_txt := URLtoText (url);
            HTML.BeginYY (NIL, wx, "Matches for ", url_txt);
            wx.put ("<H3>");
            HTML.PutImg ("what", wx); wx.put (" ");
            wx.put ("Matches for <TT>", url_txt, "</TT>");
            wx.put ("</H3>\n");
            GenPathFinder (url, wx);
            HTML.GenChoices (results, wx);
            HTML.ViewOnly (url.action, url.data, wx);
            HTML.End (wx);
    END;

    (* make sure the garbage collector gets a chance... *)
    results.elts := NIL;
  END ProcessRequest;

PROCEDURE Clean (req: TEXT): TEXT =
  (* strip any trailing nasty characters... *)
  VAR i := Text.Length (req);  c: CHAR;
  BEGIN
    WHILE (i > 0) DO
      DEC (i);
      c := Text.GetChar (req, i);
      IF (c # '\n') AND (c # '\r') THEN EXIT; END;
    END;
    RETURN Text.Sub (req, 0, i+1);
  END Clean;

PROCEDURE GenLocation (READONLY url: URL;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    wx.put ("Location: " (**, Default.server_href**));
    FOR i := 0 TO url.n_arcs - 1 DO
      wx.put ("/", url.arcs[i].pattern);
    END;
    IF (url.action # viewID) AND (url.action # ID.NoID) THEN
      wx.put ("[", ID.ToText (url.action), "]");
    END;
    wx.put ("\n");
  END GenLocation;

PROCEDURE URLtoText (READONLY url: URL): TEXT =
  VAR txt := "/";
  BEGIN
    FOR i := 0 TO url.n_arcs - 1 DO
      txt := txt & url.arcs[i].pattern & "/";
    END;
    IF (url.action # viewID) AND (url.action # ID.NoID) THEN
      txt := txt & "[" & ID.ToText (url.action) & "]";
    END;
    RETURN txt;
  END URLtoText;

PROCEDURE MatchingURL (n: Node.T;  READONLY url: URL): BOOLEAN =
  VAR
    arcs : ARRAY [0..19] OF Node.T;
    len  := Node.FindArcs (n, arcs);
    node_arc, url_arc : TEXT;
  BEGIN
    IF NOT url.query_merge THEN RETURN TRUE; END;
    IF (len # url.n_arcs) THEN RETURN FALSE; END;
    FOR i := 0 TO len-1 DO
      node_arc := ID.ToText (arcs[i].arcname ());
      url_arc  := url.arcs[i].pattern;
      IF (node_arc = NIL) OR (url_arc = NIL)
        OR NOT Text.Equal (node_arc, url_arc) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END MatchingURL;

(*-------------------------------------------------------- query parsing ---*)

PROCEDURE ParseRequest (cmd: TEXT;  VAR(*OUT*) url: URL)
  RAISES {Error} =
  VAR
    len  := Text.Length (cmd);
    buf0 : Buf.T;
    buf1 : ARRAY [0..127] OF CHAR;
  BEGIN
  (* begin patch by Olaf Wagner to fix problem whereby
     some browsers have already escaped part of the URL.
     This patch also necessitates a change in ParseArcs. *)
    TRY
      cmd := UnescapeURLEntry( cmd );
    EXCEPT
      Error => Err( "cannot decode URL: " & cmd );
    END;
  (* end of Olaf's patch *)
  
    IF (len <= NUMBER (buf1)) THEN
      Text.SetChars (buf1, cmd);
      ParseBuf (SUBARRAY (buf1, 0, len), url);
    ELSE
      buf0 := Buf.FromText (cmd);
      ParseBuf (buf0^, url);
    END;
  END ParseRequest;

PROCEDURE ParseBuf (VAR(*INOUT*) buf: ARRAY OF CHAR;  VAR(*OUT*) url: URL)
  RAISES {Error} =
  VAR
    len := NUMBER (buf);
    start,   end  : INTEGER;
    q_start, q_end: INTEGER;
    a_start, a_end: INTEGER;
  BEGIN
    url.action := ID.NoID;
    url.data   := NIL;
    url.n_arcs := 0;
    url.query_merge := FALSE;

    (* check the fixed header *)
    IF (len <= 5) THEN Err ("too short"); END;
    IF (buf[0] # 'G') OR (buf[1] # 'E') OR (buf[2] # 'T')
      OR (buf[3] # ' ') OR (buf[4] # '/') THEN
      Err ("unrecognized command");
    END;

    (* strip off the trailing " HTTP/1.0\r\n" *)
    WHILE (len > 0) AND (buf[len-1] # ' ') DO DEC (len); END;

    (* skip leading white space *)
    start := 5;
    WHILE (start < len) AND IsBlank [buf [start]] DO INC (start); END;

    (* find the end of the request *)
    end := start;
    WHILE (end < len) AND NOT IsBlank [buf [end]] DO INC (end); END;

    IF (start >= end) THEN
      (* the empty URL => "/rsrc/index.html" *)
      AddRootArcs (url);
      url.action := viewID;
      RETURN;
    END;

    (* find the attached query *)
    q_start := start;  q_end := end;
    WHILE (q_start < end) AND (buf[q_start] # '?') DO INC (q_start); END;
    IF (q_start < end) THEN
      end := q_start;
      INC (q_start);
    END;

    (* look for an appended action *)
    a_start := end-1;  a_end := a_start;
    IF (a_start > start) AND (buf[a_start] = ']')
      AND (buf[a_start-1] # BackSlash) THEN
      DEC (a_start);
      LOOP
        IF (a_start < start) THEN
          (* didn't find a starting bracket *)
          a_start := end; EXIT;
        ELSIF (buf[a_start] # '[') THEN
          DEC (a_start);
        ELSIF (a_start = start) OR (buf[a_start-1] # BackSlash) THEN
          end := a_start - 1; INC (a_start); EXIT;
        ELSE
          DEC (a_start);
        END;
      END;
    END;

    ParseArcs  (buf, start, end, url);
    ParseAction (buf, a_start, a_end, url);
    ParseQuery (buf, q_start, q_end, url);
  END ParseBuf;

PROCEDURE AddRootArcs (VAR url: URL)
  RAISES {Error} =
  VAR user_home := ConfigItem.X [ConfigItem.T.Homepage].text;
  BEGIN
    IF (user_home # NIL) AND Text.Length (user_home) > 0 THEN
      AddArcText ("user", url);  AddArcText (Pathname.Last (user_home), url);
    ELSE
      AddArcText ("rsrc", url);  AddArcText ("start.html", url);
    END;
  END AddRootArcs;

PROCEDURE ParseArcs (VAR buf: ARRAY OF CHAR;  start, end: INTEGER;  VAR url: URL)
  RAISES {Error} =
  VAR s0, s1, s2 := start;  (* c: CHAR; *)
  BEGIN
    WHILE (s2 < end) DO
      IF (buf[s2] = '/') THEN
        (* end of the current arc *)
        AddArc (buf, s0, s1, url);  INC (s2);  s0 := s2;  s1 := s0;
      ELSIF (buf[s2] = BackSlash) AND (s2+1 < end) AND (buf[s2+1] = '/') THEN
        (* escaped slash *)
        buf[s1] := '/';  INC (s1);  INC (s2, 2);

(* *** This code disabled; the URL has already been unescaped via 
 *     the patch in ParseRequest ***
 *    ELSIF (buf[s2] = '%') AND (s2+2 < end) THEN
 *      (* grab the next two letters and build the ascii character *)
 *      c := HexChar (buf[s2+1], buf[s2+2]);  INC (s2, 3);
 *      IF (c = '/') THEN
 *        AddArc (buf, s0, s1, url);  s0 := s2;  s1 := s0;
 *      ELSE
 *        buf[s1] := c;  INC (s1);
 *      END;
 * *** *)
      ELSE
        (* regular character *)
        buf[s1] := buf[s2];  INC (s1);  INC (s2);
      END;
    END;
    AddArc (buf, s0, s1, url);
  END ParseArcs;

PROCEDURE ParseAction (VAR buf: ARRAY OF CHAR;  start, end: INTEGER;
                       VAR url: URL) =
  BEGIN
    IF (start >= end)
      THEN url.action := viewID;
      ELSE url.action := ID.FromStr (SUBARRAY (buf, start, end - start));
    END;
  END ParseAction;

TYPE
  HTTPQuery = REF RECORD
    fieldname : TEXT       := NIL;  (* or NIL for a pure ISINDEX query *)
    words     : TextList.T := NIL;
    next      : HTTPQuery  := NIL;
  END;

PROCEDURE ParseQuery (VAR buf: ARRAY OF CHAR;  start, end: INTEGER;  VAR url: URL)
  RAISES {Error} =
  VAR query := ParseHTTPQuery (buf, start, end);
  BEGIN
    url.data := NIL;

    IF (start < end) AND (buf [start] = '/') THEN
      (* it appears that the query is really a rooted URL
         => nuke the existing one *)
      url.query_merge := TRUE;
      url.n_arcs := 0;
      ParseArcs (buf, start+1, end, url);
      RETURN;
    ELSIF (start < end+2) AND (buf [start] = '%')
     AND (buf [start+1] = '2')  AND (buf [start+2] = 'F') THEN
      (* it appears that the query is really a rooted URL
         => nuke the existing one *)
      url.query_merge := TRUE;
      url.n_arcs := 0;
      ParseArcs (buf, start+3, end, url);
      RETURN;
    END;

    IF (query = NIL) THEN
      (* no query *)
    ELSIF (query.next = NIL) AND (query.fieldname = NIL) THEN
      (* simple query *)
      IF (query.words # NIL) THEN
        url.query_merge := TRUE;
        AddArcText (CvtWordsToRegExpr (query.words), url);
      END;
    ELSE
      (* form data *)
      url.data := CvtFormData (query);
    END;
  END ParseQuery;

PROCEDURE ParseHTTPQuery (VAR buf: ARRAY OF CHAR;  start, end: INTEGER): HTTPQuery
  RAISES {Error} =
  VAR
    s0    : INTEGER;
    xlen  : INTEGER := 0;
    xx    : ARRAY [0..127] OF CHAR;
    query : HTTPQuery := NIL;
    word  : TEXT;

  PROCEDURE AddCh (ch: CHAR) RAISES {Error} =
    BEGIN
      IF (xlen >= NUMBER (xx)) THEN Err ("query word too long"); END;
      xx[xlen] := ch;  INC (xlen);
    END AddCh;

  PROCEDURE GetWord () =
    BEGIN
      word := NIL;
      IF (xlen > 0) THEN
        word := Text.FromChars (SUBARRAY (xx, 0, MAX(0, xlen)));
        xlen := 0;
      END;
    END GetWord;

  BEGIN
    IF (start >= end) THEN RETURN NIL; END;
    query := NEW (HTTPQuery);
    s0 := start;
    WHILE (s0 < end) DO

      IF (buf[s0] = '+') THEN
        (* end of the current word *)
        GetWord ();
        IF (word # NIL) THEN
          query.words := TextList.Cons (word, query.words);
        END;
        INC (s0);

      ELSIF (buf[s0] = '=') THEN
        (* end of the field name *)
        GetWord ();
        query.fieldname := word;
        INC (s0);

      ELSIF (buf[s0] = '&') THEN
        (* end of the current field's value *)
        GetWord ();
        IF (word # NIL) THEN
          query.words := TextList.Cons (word, query.words);
        END;
        IF (query.fieldname # NIL) OR (query.words # NIL) THEN
          query := NEW (HTTPQuery, next := query);
        END;
        INC (s0);

      ELSIF (buf[s0] = '%') AND (s0+2 < end) THEN
        (* grab the next two letters and build the ascii character *)
        AddCh (HexChar (buf[s0+1], buf[s0+2]));  INC (s0, 3);

      ELSE
        (* regular character *)
        AddCh (buf[s0]);  INC (s0);
      END;
    END;

    (* grab the last word *)
    GetWord ();
    IF (word # NIL) THEN
      query.words := TextList.Cons (word, query.words);
    END;

    IF (query.fieldname = NIL) AND (query.words = NIL) AND (query.next = NIL) THEN
      (* we didn't get anything useful! *)
      query := NIL;
    END;

    RETURN FixupQuery (query);
  END ParseHTTPQuery;

PROCEDURE FixupQuery (a: HTTPQuery): HTTPQuery =
  VAR b, c: HTTPQuery := NIL;
  BEGIN
    WHILE (a # NIL) DO
      a.words := TextList.ReverseD (a.words);
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;
    RETURN b;
  END FixupQuery;

(***
PROCEDURE DumpQuery (x: HTTPQuery) =
  VAR words: TEXT;  zz: TextList.T;
  BEGIN
    IF (x = NIL) THEN RETURN; END;
    ErrLog.Msg ("---- QUERY ----");
    WHILE (x # NIL) DO
      ErrLog.Msg ("field: \"", x.fieldname, "\"");
      words := NIL;
      zz := x.words;
      WHILE (zz # NIL) DO
        IF (words = NIL)
          THEN words := zz.head;
          ELSE words := words & " " & zz.head;
        END;
        zz := zz.tail;
      END;
      ErrLog.Msg ("  val: \"", words, "\"");
      x := x.next;
    END;
  END DumpQuery;
**)

PROCEDURE CvtWordsToRegExpr (words: TextList.T): TEXT =
  (* Plan: build a regular expression which is the conjunction of
     the query words and add it as an arc *)
  BEGIN
    IF words = NIL THEN
      RETURN "";
    ELSIF (words.tail = NIL) AND (words.head # NIL) THEN
      RETURN words.head;
    ELSE
      RETURN FlattenWords (words, "(", "&", ")");
    END;
  END CvtWordsToRegExpr;

PROCEDURE CvtFormData (query: HTTPQuery): Node.FormData =
  VAR result: Node.FormData := NIL;
  BEGIN
    WHILE (query # NIL) DO
      result := NEW (Node.FormData, next := result, field := query.fieldname,
                     value := FlattenWords (query.words, NIL, " ", NIL));
      query := query.next;
    END;
    RETURN ReverseD (result);
  END CvtFormData;

PROCEDURE FlattenWords (words: TextList.T;  pre, mid, post: TEXT): TEXT =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR buf := NEW (Wx.T).init(NIL);  w := words;
  BEGIN
    WHILE (w # NIL) DO
      IF (w.head # NIL) THEN
        IF (w # words) THEN buf.put (mid); END;
        buf.put (pre);
        buf.put (w.head);
        buf.put (post);
      END;
      w := w.tail;
    END;
    RETURN buf.toText ();
  END FlattenWords;

PROCEDURE ReverseD (a: Node.FormData): Node.FormData =
  VAR b, c: Node.FormData := NIL;
  BEGIN
    WHILE (a # NIL) DO
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;
    RETURN b;
  END ReverseD;

PROCEDURE AddArc (READONLY buf: ARRAY OF CHAR;  start, end: INTEGER; 
                  VAR url: URL)
  RAISES {Error} =
  BEGIN
    IF (start < end) THEN
      AddArcText (Text.FromChars (SUBARRAY (buf, start, end - start)), url);
    END;
  END AddArc;

PROCEDURE AddArcText (txt: TEXT;  VAR url: URL)
  RAISES {Error} =
  BEGIN
    IF (url.n_arcs >= NUMBER (url.arcs)) THEN Err ("too many arcs"); END;
    WITH z = url.arcs [url.n_arcs] DO
      TRY
        z.count   := 0;
        z.pattern := txt;
        z.expr    := RegExpr.Compile (txt);  INC (url.n_arcs);
      EXCEPT RegExpr.Error (msg) =>
        Err ("invalid regular expression \"" & txt & "\": " & msg);
      END;
    END;
  END AddArcText;

PROCEDURE Err (msg: TEXT) RAISES {Error} =
  BEGIN
    RAISE Error (msg);
  END Err;

PROCEDURE HexChar (a, b: CHAR): CHAR =
  VAR n := 0;
  BEGIN
    IF    ('0' <= a) AND (a <= '9') THEN  n := ORD(a) - ORD ('0');
    ELSIF ('A' <= a) AND (a <= 'F') THEN  n := ORD(a) - ORD ('A') + 10;
    ELSIF ('a' <= a) AND (a <= 'f') THEN  n := ORD(a) - ORD ('a') + 10;
    END;
    n := n * 16;
    IF    ('0' <= b) AND (b <= '9') THEN  n := n + ORD(b) - ORD ('0');
    ELSIF ('A' <= b) AND (b <= 'F') THEN  n := n + ORD(b) - ORD ('A') + 10;
    ELSIF ('a' <= b) AND (b <= 'f') THEN  n := n + ORD(b) - ORD ('a') + 10;
    END;
    RETURN VAL (n, CHAR);
  END HexChar;

(*------------------------------------------------- result collection ---*)

PROCEDURE CollectResults (VAR url: URL;  VAR(*OUT*) results: Node.Set)
  RAISES {Thread.Alerted} =
  VAR n_roots := 0;
  BEGIN
    results.elts := NIL;
    results.cnt  := 0;
    IF (url.n_arcs <= 0) THEN RETURN END;

    (* scan for a matching root binding *)
    FOR i := 0 TO n_bindings-1 DO
      WITH b = bindings[i] DO
        IF RegExpr.Match (url.arcs[0].expr, b.tag) THEN
          INC (n_roots);
          ScanNode (b.root, url, 0, results);
        END;
      END;
    END;

    IF (n_roots <= 0) AND (default.root # NIL) THEN
      (* no root matched => use the default root *)
      ScanNode (default.root, url, 0, results);
    END;
  END CollectResults;

PROCEDURE ScanNode (n: Node.T; VAR url: URL;  depth: INTEGER;
                    VAR results: Node.Set)
  RAISES {Thread.Alerted} =
  VAR iter: Node.IteratorState;  pause := 100;
  BEGIN
    WITH z = url.arcs[depth] DO  z.hit := n;  INC (z.count);  END;
    IF (depth >= url.n_arcs-1) THEN
      (* cut off the search at this depth *)
      IF (n # NIL) THEN Node.Append (results, n); END;
      RETURN;
    END;
    iter.pattern := url.arcs[depth+1].expr;
    n.iterate (iter);
    WHILE n.next (iter) DO
      ScanNode (iter.match, url, depth+1, results);
      DEC (pause);
      IF (pause <= 0) THEN
        IF Thread.TestAlert () THEN RAISE Thread.Alerted; END;
        pause := 100;
      END;
    END;
  END ScanNode;

(*--------------------------------------------------------- no results ---*)

PROCEDURE GenPathFinder (READONLY url: URL;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR nc: Node.Class;
  BEGIN
    wx.put ("<H5>&nbsp;&nbsp;");
    wx.put ("<A HREF=\"/\">");
    HTML.PutSmallImg ("unknown", wx);
    wx.put ("</A>&nbsp;<A HREF=\"/\">CM3-IDE</A>");
    FOR i := 0 TO url.n_arcs - 1 DO
      WITH z = url.arcs[i] DO
        wx.put ("&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;");
        IF (z.count = 1) AND (z.hit # NIL) THEN
          nc := z.hit.class ();
          IF Node.ClassIcon[nc] # NIL THEN
            HTML.GenRef (z.hit, wx);
            HTML.PutSmallImg (Node.ClassIcon[nc], wx);
            wx.put ("</A>&nbsp;");
          END;
          HTML.GenRef (z.hit, wx); wx.put (z.hit.printname(), "</A>");
        ELSE
          wx.put ("<A HREF=\"");
          FOR j := 0 TO i DO  wx.put ("/", url.arcs[j].pattern); END;
          wx.put ("\">", z.pattern, "</A>");
          IF (z.count = 0) THEN EXIT; END;
        END;
      END;
    END;
    wx.put ("</H5>\n");
  END GenPathFinder;

(*--------------------------------------------------------- no results ---*)

PROCEDURE NoResults (READONLY url: URL;  wx: Wx.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR cnt: TEXT;  hit_zero := FALSE;
  BEGIN
    wx.put ("<PRE>\n");
    wx.put (" #hits   pattern\n");
    wx.put ("------   ----------\n");
    FOR i := 0 TO url.n_arcs - 1 DO
      WITH z = url.arcs[i] DO
        cnt := Fmt.Int (z.count);
        FOR j := Text.Length (cnt) TO 5 DO wx.putChar (' '); END;
        wx.put (cnt, "  ");
        FOR j := 0 TO i+i DO  wx.putChar (' ');  END;
        wx.put ("<A HREF=\"");
        FOR j := 0 TO i DO  wx.put ("/", url.arcs[j].pattern); END;
        wx.put ("/\">/", z.pattern, "</A>\n");
        IF (z.count = 0) THEN hit_zero := TRUE;  EXIT; END;
      END;
    END;
    IF (NOT hit_zero) THEN
      wx.put ("     0\n");
    END;
    wx.put ("</PRE>\n");
  END NoResults;

(*----------------------------------------------------- initialization ---*)

PROCEDURE Init () =
  BEGIN
    viewID := ID.Add ("view");
    FOR i := FIRST (IsBlank) TO LAST (IsBlank) DO IsBlank[i] := FALSE; END;
    IsBlank [' ']  := TRUE;
    IsBlank ['\r'] := TRUE;
    IsBlank ['\t'] := TRUE;
    IsBlank ['\n'] := TRUE;
  END Init;

BEGIN
  Init ();
END WebServer.

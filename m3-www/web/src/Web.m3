(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Wed Nov 29 12:47:57 PST 1995 by mhb      *)
(*      modified on Thu Sep  7 15:56:11 PDT 1995 by najork   *)
(*      modified on Wed Sep  6 11:03:30 PDT 1995 by glassman *)

MODULE Web;

IMPORT ASCII, ConnRW, Date, Env, FloatMode, Fmt, IP, Lex, Rd,
       TextList, TCP, Text, TextF, TextRd, TextWr, Thread, Wr;

<* FATAL FloatMode.Trap, Rd.Failure *>

CONST
  ProxyVar         = "http_proxy";
  NoProxyVar       = "no_proxy";
    (* This is the HTTP marker for "end of this line *)
  EndOfRequest = "\r\n";

TYPE MethodType = {Head, Get, GetDebug, Post};

REVEAL
  T = ROOT BRANDED OBJECT
        host   : TEXT;
        port   : INTEGER;
        noProxy: TextList.T;
      END;

VAR
  mu               := NEW(MUTEX);
  DefaultServer: T := NIL;


(*-------------------- TCP stuff --------------------*)

PROCEDURE ChannelPut (channel: TCP.T; text: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    len                            := Text.Length(text);
    buf: ARRAY [0 .. 2047] OF CHAR;
  BEGIN
    <* ASSERT len < NUMBER(buf) *>
    Text.SetChars(buf, text);
    channel.put(SUBARRAY(buf, 0, len));
  END ChannelPut;

PROCEDURE NoProxyMatch (host, suffix: TEXT): BOOLEAN =
  VAR
    hostLen   := Text.Length(host);
    suffixLen := Text.Length(suffix);
  BEGIN
    IF hostLen < suffixLen THEN RETURN FALSE END;
    FOR i := 1 TO suffixLen DO
      IF Text.GetChar(host, hostLen - i)
           # Text.GetChar(suffix, suffixLen - i) THEN
        RETURN FALSE
      END;
    END;
    RETURN TRUE;
  END NoProxyMatch;

PROCEDURE OpenTCPConnect (server: T; VAR url: TEXT): TCP.T
  RAISES {IP.Error, Thread.Alerted, Error} =
  VAR
    addr            : IP.Address;
    channel         : TCP.T;
    host                         := server.host;
    port                         := server.port;
    list                         := server.noProxy;
    urlHost, urlPath: TEXT       := NIL;
    urlPort         : INTEGER    := 80;
  BEGIN
    URLHostPort(url, urlHost, urlPort, urlPath);

    IF server.host = NIL THEN
      host := urlHost;
      port := urlPort;
      url := urlPath;
    ELSE
      WHILE list # NIL DO
        IF NoProxyMatch(urlHost, Pop(list)) THEN
          host := urlHost;
          port := urlPort;
          url := urlPath;
        END;
      END;
    END;

    IF IP.GetHostByName(host, addr) THEN
      channel := TCP.Connect(IP.Endpoint{addr, port});
    ELSE
      RAISE IP.Error(NIL);
    END;
    RETURN channel;
  END OpenTCPConnect;


(*-------------------- Exported procedures --------------------*)

(* port must be initialized to default value *)
(* splits URL into host, port, pathname *)

PROCEDURE URLHostPort (                 url : TEXT;
                       VAR (* OUT *)    host: TEXT;
                       VAR (* IN/OUT *) port: INTEGER; 
                       VAR (* OUT *)    path: TEXT     )
  RAISES {Thread.Alerted, Error} =
  VAR rd := TextRd.New(url);
  BEGIN
    TRY
      TRY
        EVAL Lex.Scan(rd, NonColon); (* throw away http:// *)
        Lex.Skip(rd, Seps);
        host := Lex.Scan(rd, NonColon);
        IF Rd.GetChar(rd) = ':' THEN (* port ; otherwise default *)
          port := Lex.Int(rd); 
        ELSE
          Rd.UnGetChar(rd);
        END;
        path := Rd.GetText(rd, 300);
      EXCEPT
        Lex.Error => RAISE Error(Fmt.F("Lexing error parsing %s\n", url));
      | Rd.EndOfFile =>
          path := "/";           (* okay; probably just an http://host/ *)
      END;
    FINALLY
      Rd.Close(rd);
    END;
  END URLHostPort;

PROCEDURE Setup (proxyURL, noProxyList: TEXT := NIL): T RAISES {Error} =
  VAR
    server                 := NEW(T, port := 8080);
    rd          : TextRd.T;
    txt, noProxy: TEXT;
    garbage     : TEXT;
  <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    IF proxyURL = NIL THEN
      proxyURL := Env.Get(ProxyVar);
    END;
    IF proxyURL = NIL THEN 
      proxyURL := DefaultProxyHost; 
    END;
    IF proxyURL = NIL THEN
      server.host := NIL;
    ELSE
      URLHostPort(proxyURL, server.host, server.port, garbage);
    END;

    IF noProxyList = NIL THEN
      noProxy := Env.Get(NoProxyVar);
      IF noProxy = NIL THEN noProxy := DefaultNoProxyList; END;
      TRY
        rd := TextRd.New(noProxy);
        LOOP
          txt := Lex.Scan(rd, NoProxyChars);
          IF Text.Length(txt) = 0 THEN EXIT END;
          server.noProxy := TextList.Cons(txt, server.noProxy);
          Lex.Skip(rd, NoProxySeps);
        END;
      FINALLY
        Rd.Close(rd);
      END;
    END;
    RETURN server
  END Setup;

PROCEDURE Get (    url   : TEXT;
               VAR header: Header;
               READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
               forceCache, debug: BOOLEAN := FALSE;
               server           : T       := NIL    ): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error} =
  BEGIN
    IF debug THEN
      RETURN InternalDoRequest(MethodType.GetDebug, url, NIL, header,
                               requestFields, forceCache, server);
    ELSE
      RETURN InternalDoRequest(MethodType.Get, url, NIL, header,
                               requestFields, forceCache, server);
    END;
  END Get;

(* forceCache is meaningless on POST calls *)
PROCEDURE Post (    url      : TEXT;
                    argString: TEXT;
                VAR header   : Header;
                READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
                server: T := NIL): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error} =
  BEGIN
    RETURN InternalDoRequest(MethodType.Post, url, argString, header,
                             requestFields, FALSE, server);
  END Post;


PROCEDURE GetHead (url: TEXT;
                   READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
                   forceCache: BOOLEAN := FALSE;
                   server    : T       := NIL    ): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error} =
  VAR header: Header;
  BEGIN
    RETURN InternalDoRequest(
             MethodType.Head, url, NIL, header, requestFields, forceCache, server);
  END GetHead;


(*-------------------- Internal routine for Get, GetHead, Post --------------------*)

PROCEDURE InternalDoRequest (    method        : MethodType;
                               url           : TEXT;
                               postMethodArgs: TEXT         := NIL;
                           VAR header        : Header;
                           READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
                           forceCache: BOOLEAN := FALSE;
                           server    : T       := NIL    ): Rd.T
  RAISES {Error, Thread.Alerted, IP.Error} =
  VAR
    channel: TCP.T;
    rd     : Rd.T;
  <* FATAL Wr.Failure *>
  BEGIN
    IF server = NIL THEN
      LOCK mu DO
        IF DefaultServer = NIL THEN DefaultServer := Setup() END;
        server := DefaultServer
      END;
    END;
    channel := OpenTCPConnect(server, url);
    CASE method OF
    | MethodType.Get, MethodType.GetDebug =>
        ChannelPut(channel, Fmt.F("GET %s HTTP/1.0%s", url, EndOfRequest));
    | MethodType.Post =>
        ChannelPut(channel, Fmt.F("POST %s %s HTTP/1.0%s", url,
                                  postMethodArgs, EndOfRequest));
    | MethodType.Head =>
        ChannelPut(channel, Fmt.F("HEAD %s HTTP/1.0%s", url, EndOfRequest));
    END;

    FOR i := 0 TO LAST(requestFields) DO
      ChannelPut(channel, Fmt.F("%s%s", requestFields[i], EndOfRequest));
    END;
    IF forceCache THEN
      ChannelPut(channel, Fmt.F("Pragma: no-cache%s", EndOfRequest));
    END;
    ChannelPut(channel, EndOfRequest);
    rd := ConnRW.NewRd (channel);
    IF method = MethodType.Head OR method = MethodType.GetDebug THEN
      RETURN rd;
    END;
    header := ParseHead(rd);
    RETURN rd;
  END InternalDoRequest;


(*-------------------- Fun with Lex --------------------*)

CONST
  NonColon     = SET OF CHAR{'\000'.. '~'} - SET OF CHAR{':', '/', '\n'};
  ColonSpace   = SET OF CHAR{' ', '\t', ':', ','};
  Seps         = SET OF CHAR{':', '/'};
  NoProxySeps  = SET OF CHAR{',', ' ', '\n'};
  NoProxyChars = SET OF CHAR{'\000'.. '~'} - NoProxySeps;

(* format is HTTP/1.0 200 OK.  If header is gibberish, that means the
   server isn't responding. *)
PROCEDURE SplitStatusLine (rd: Rd.T; VAR h: Header)
  RAISES {Thread.Alerted} =
  BEGIN
    TRY
      h.httpVersion := Lex.Scan(rd);
      Lex.Skip(rd);
      h.statusCode := Lex.Int(rd);
      Lex.Skip(rd);
      h.reason := Rd.GetLine(rd);
      Lex.Skip(rd);
    EXCEPT
      Rd.Failure, Lex.Error, Rd.EndOfFile =>
        h.statusCode := 404;
        h.reason :=
          "The information server either is not accessible or is refusing to serve the document to you.";
    END;
  END SplitStatusLine;

PROCEDURE GetContentType (rd: Rd.T; VAR type: MIMEType; VAR subtype: TEXT)
  RAISES {Thread.Alerted, Error} =
  VAR t: TEXT;
  BEGIN
    TRY
      t := Lex.Scan(rd, SET OF CHAR{'A'.. 'Z', 'a'.. 'z'});
      IF CIEqual(t, "text") THEN
        type := MIMEType.Text
      ELSIF CIEqual(t, "multipart") THEN
        type := MIMEType.Multipart;
      ELSIF CIEqual(t, "message") THEN
        type := MIMEType.Message;
      ELSIF CIEqual(t, "image") THEN
        type := MIMEType.Image;
      ELSIF CIEqual(t, "audio") THEN
        type := MIMEType.Audio;
      ELSIF CIEqual(t, "video") THEN
        type := MIMEType.Video;
      ELSIF CIEqual(t, "application") THEN
        type := MIMEType.Application;
      ELSIF CIEqual(Text.Sub(t, 0, 2), "X-") THEN
        type := MIMEType.Xperimental;
      ELSE
        RAISE
          Error(
            Fmt.F(
              "Unrecognized MIME type ``%s''in content-type field\n", t));
      END;
      EVAL Rd.GetChar(rd);
      subtype := Lex.Scan(rd);
      EVAL Rd.GetLine(rd);
    EXCEPT
      Rd.Failure, Rd.EndOfFile =>
        RAISE Error("Error in content-type field\n");
    END;
  END GetContentType;

(* End of header marker is vague: should be \r\n\r\n, but experimentally
   "two or more carriage returns separated by linefeed" seems to be about
   the best I can do. *)
PROCEDURE ParseHead (rd: Rd.T): Header RAISES {Error, Thread.Alerted} =
  VAR
    h    : Header;
    label: TEXT;
  <* FATAL Rd.Failure *>
  BEGIN
    TRY
      SplitStatusLine(rd, h);
      LOOP
        label := Lex.Scan(rd, NonColon);
        Lex.Skip(rd, ColonSpace);

        IF label = NIL OR Text.Length(label) = 0
             OR Text.GetChar(label, 0) = '\r' THEN
          Lex.Skip(rd);
          EXIT
          (* end of header, we hope *)
        ELSIF CIEqual(label, "Allowed") OR CIEqual(label, "Allow") THEN
          h.allowed := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Public") THEN
          h.public := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Content-Length") THEN
          h.contentLength := Lex.Int(rd);
          EVAL Rd.GetLine(rd);
        ELSIF CIEqual(label, "Content-Encoding") THEN
          h.encoding := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Content-type") THEN
          GetContentType(rd, h.contentType, h.contentSubType);
        ELSIF CIEqual(label, "Date") THEN
          h.date := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Expires") THEN
          h.expires := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Last-Modified") THEN
          h.lastModified := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Server") THEN
          h.server := Rd.GetLine(rd);
        ELSIF CIEqual(label, "MIME-version") THEN
          h.MIMEVersion := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Title") THEN
          h.title := Rd.GetLine(rd);
        ELSIF CIEqual(label, "Location") THEN
          h.location := Rd.GetLine(rd);
        ELSE
          (* pitch 'em *)
          EVAL Rd.GetLine(rd);
        END;
      END;
    EXCEPT
    | Lex.Error, Rd.EndOfFile =>
        RAISE Error("Error while parsing http header");
    END;
    RETURN h;
  END ParseHead;


PROCEDURE AbsoluteURL (url: TEXT; base: TEXT): TEXT =
  VAR i: INTEGER;
  BEGIN

    IF url = NIL OR Text.Empty(url) THEN RETURN NIL END;
    IF base = NIL OR Text.Empty(base) THEN RETURN url END;

    (* if the URL starts with foo:// or file:/, it's OK; otherwise, 
       strip off the foo: *)

    i := Text.FindChar(url, ':');
    IF i # -1 AND Text.Length(url) > i + 2 THEN
      IF Text.GetChar(url, i + 1) # '/' OR 
         (Text.GetChar(url, i + 2) # '/' AND
          (NOT Text.Equal("file",Text.Sub(url,0,i)))) THEN
        url := Text.Sub(url, i + 1)
      END
    END;

    IF Text.GetChar(url, 0) = '/' THEN
      (* Relative to base's host. *)
      i := Text.FindChar(base, ':');
      IF i # -1 AND Text.Equal("file",Text.Sub(base,0,i)) THEN
        url := "file:" & url;
      ELSE
        i := Text.FindChar(base, '/', i + 3);  
        IF i = -1 THEN 
           (* base doesn't end with a slash *)
           url := base & url
        ELSE   
           url := Text.Sub(base, 0, i) & url 
        END;
      END;

(* internal ref; don't want slash *)
    ELSIF Text.GetChar(url, 0) = '#' THEN 
      url := base & url;

    ELSIF Text.FindChar(url, ':', 0) # -1 THEN
      (* Absolute form already. *)

    ELSE
      (* Relative to base's host and directory. *)
      i := Text.FindCharR(base, '/');
      url := Text.Sub(base, 0, i) & "/" & url;
    END;

    (* Add trailing slash if need be. *)
    i := Text.FindChar(url, ':', 0);
    IF Text.FindChar(url, '/', i + 3) = -1 THEN url := url & "/"; END;

    RETURN url;
  END AbsoluteURL;

(* Brute strength and ignorance. *)
PROCEDURE EncodeURL (t: TEXT): TEXT RAISES {Thread.Alerted} =
  VAR
    c : CHAR;
    rd       := TextRd.New(t);
    wr       := TextWr.New();
  <* FATAL Wr.Failure *>
  BEGIN
    TRY
      LOOP
        c := Rd.GetChar(rd);
        CASE c OF
        | ' ' => Wr.PutChar(wr, '+');
        | '!' => Wr.PutText(wr, "%21");
        | '"' => Wr.PutText(wr, "%22");
        | '#' => Wr.PutText(wr, "%23");
        | '$' => Wr.PutText(wr, "%24");
        | '%' => Wr.PutText(wr, "%25");
        | '&' => Wr.PutText(wr, "%26");
        | '\'' => Wr.PutText(wr, "%27");
        | '(' => Wr.PutText(wr, "%28");
        | ')' => Wr.PutText(wr, "%29");
        | '+' => Wr.PutText(wr, "%2B");
        | ',' => Wr.PutText(wr, "%2C");
        | '/' => Wr.PutText(wr, "%2F");
        | ':' => Wr.PutText(wr, "%3A");
        | ';' => Wr.PutText(wr, "%3B");
        | '<' => Wr.PutText(wr, "%3C");
        | '=' => Wr.PutText(wr, "%3D");
        | '>' => Wr.PutText(wr, "%3E");
        | '?' => Wr.PutText(wr, "%3F");
        | '[' => Wr.PutText(wr, "%5B");
        | '\\' => Wr.PutText(wr, "%5C");
        | ']' => Wr.PutText(wr, "%5D");
        | '^' => Wr.PutText(wr, "%5E");
        | '{' => Wr.PutText(wr, "%7B");
        | '|' => Wr.PutText(wr, "%7C");
        | '}' => Wr.PutText(wr, "%7D");
        | '~' => Wr.PutText(wr, "%7E");
        ELSE
          Wr.PutChar(wr, c);
        END;
      END;
    EXCEPT
      Rd.EndOfFile => Rd.Close(rd); RETURN TextWr.ToText(wr);
    END;
  END EncodeURL;


(* ----------------------------- Date parsing -------*)

(* Date format is : Monday, 12-Dec-94 04:02:35 GMT *)

PROCEDURE ToDate (t: HTMLDate): Date.T RAISES {Error, Thread.Alerted} =
  VAR
    d : Date.T;
    rd: Rd.T   := NIL;
  BEGIN
    TRY
      rd := TextRd.New(t);
      d.weekDay := GetDay(Lex.Scan(rd)); (* includes the trailing comma *)
      Lex.Skip(rd);
      d.day := Lex.Int(rd);
      EVAL Rd.GetChar(rd);       (* hyphen *)
      d.month := GetMonth(Lex.Scan(rd, SET OF CHAR{'A'.. 'Z', 'a'.. 'z'}));
      EVAL Rd.GetChar(rd);       (* hyphen *)
      d.year := 1900 + Lex.Int(rd);
      Lex.Skip(rd);              (* hyphen *)
      d.hour := Lex.Int(rd);
      EVAL Rd.GetChar(rd);
      d.minute := Lex.Int(rd);
      EVAL Rd.GetChar(rd);
      d.second := Lex.Int(rd);
      Lex.Skip(rd);
      d.zone := Lex.Scan(rd);    (* time zone *)
      d.offset := 0;             (* make sure it's GMT *)
      Rd.Close(rd);
      RETURN d;
    EXCEPT
      Rd.EndOfFile, Lex.Error =>
        Rd.Close(rd);
        RAISE Error(Fmt.F("Error parsing date string %s\n", t));
    | Error (e) =>
        RAISE Error(Fmt.F("Error %s parsing date string %s\n", e, t));
    END;
  END ToDate;

PROCEDURE GetMonth (t: TEXT): Date.Month RAISES {Error} =
  CONST
    Months = ARRAY OF
               TEXT{"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                    "Sep", "Oct", "Nov", "Dec"};
  BEGIN
    FOR i := 0 TO LAST(Months) DO
      IF Text.Equal(t, Months[i]) THEN RETURN VAL(i, Date.Month); END;
    END;
    RAISE Error(Fmt.F("Can't parse month %s\n", t));
  END GetMonth;

PROCEDURE GetDay (t: TEXT): Date.WeekDay RAISES {Error} =
  CONST
    Days = ARRAY OF
             TEXT{"Sunday,", "Monday,", "Tuesday,", "Wednesday,",
                  "Thursday,", "Friday,", "Saturday,"};
  BEGIN
    IF t = NIL OR Text.Length(t) < 4 THEN
      RAISE Error("Empty or truncated day");
    END;
    FOR i := 0 TO LAST(Days) DO
      IF Text.Equal(t, Days[i]) THEN RETURN VAL(i, Date.WeekDay); END;
    END;
    RAISE Error(Fmt.F("Can't parse day %s\n", t));
  END GetDay;


(*--------------Utility -------------------- *)

PROCEDURE Pop (VAR list: TextList.T): TEXT =
  VAR car := list.head;
  BEGIN
    list := list.tail;
    RETURN car;
  END Pop;

PROCEDURE CIEqual (t, u: TEXT): BOOLEAN RAISES {} =
  BEGIN
    IF t[0] # u[0] THEN RETURN FALSE END;
    VAR
      lt: CARDINAL := Text.Length(t);
      lu: CARDINAL := Text.Length(u);
      i : CARDINAL := 0;
    BEGIN
      IF lt = lu THEN
        WHILE i < lt DO
          IF ASCII.Upper[t[i]] # ASCII.Upper[u[i]] THEN
            RETURN FALSE
          ELSE
            INC(i)
          END;
        END;
        RETURN TRUE;
      ELSE
        RETURN FALSE
      END;
    END
  END CIEqual;

BEGIN
END Web.


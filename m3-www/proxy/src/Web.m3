(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Mon Oct 17 22:37:44 PDT 1994 by glassman *)
(*      modified on Thu Sep 29 12:05:53 PDT 1994 by mhb      *)

MODULE Web;

IMPORT ConnFD, Env, FilePosix, FileRd, FloatMode, Fmt, IP, Lex,
       OSError, Rd, RdClass, TCP, TCPPosix, Text, TextRd, Thread,
       Wr;

<* FATAL FloatMode.Trap *>

CONST
  EnvironmentVar   = "http_proxy";
  DefaultProxyHost = "http://<local proxy>:<proxy port>/";

REVEAL
  T = ROOT BRANDED OBJECT 
     host: TEXT; 
     port: INTEGER 
  END;

VAR 
  DefaultServer: T := NIL;

TYPE
  TCPRd = FileRd.T OBJECT
            channel: TCP.T;
          OVERRIDES
            close := Close;
          END;

CONST
  NonColon =  Lex.NonBlanks - SET OF CHAR {':'};
  Seps = SET OF CHAR {'/'};


(*--------------TCP stuff -------------------- *)

PROCEDURE ChannelPut (channel: TCP.T; text: TEXT)
  RAISES {ConnFD.TimedOut, IP.Error, Wr.Failure, Thread.Alerted} =
  VAR
    len := Text.Length(text);
    buf: ARRAY [0 .. 2047] OF CHAR;
  BEGIN
    <* ASSERT len < NUMBER(buf) *>
    Text.SetChars(buf, text);
    channel.put(SUBARRAY(buf, 0, len));
  END ChannelPut;

PROCEDURE OpenTCPConnect (server: T): TCP.T
  RAISES {ConnFD.TimedOut, IP.Error, ConnFD.TimedOut, Rd.Failure,
          Wr.Failure, Thread.Alerted, Error} =
  VAR addr: IP.Address; channel: TCP.T;
  BEGIN
    IF IP.GetHostByName(server.host, addr) THEN
      channel := TCP.Connect(IP.Endpoint{addr, server.port});
    ELSE
      RAISE IP.Error(NIL);
    END;
    RETURN channel;
  END OpenTCPConnect;

PROCEDURE Close (self: TCPRd) RAISES {Rd.Failure} =
  BEGIN
    (* Don't need to explicitly do FileRd.Close; TCP.Close and
       FileRd.Close are both just Unix.close *)
    TCP.Close(self.channel);
  END Close;


(* ---------------------- Exported procedures ------------- *)

PROCEDURE Setup (host: TEXT := NIL): T RAISES {Error} =
  VAR server := NEW(T); rd : TextRd.T;
  <* FATAL Rd.Failure, Thread.Alerted *>
  BEGIN
    TRY
      IF host = NIL THEN
        host := Env.Get(EnvironmentVar);
        IF host = NIL THEN host := DefaultProxyHost; END;
      END;
      rd := TextRd.New(host);
      EVAL Lex.Scan(rd, NonColon); (* throw away http:// *)
      Lex.Skip(rd, Seps);
      server.host := Lex.Scan(rd, NonColon);
      EVAL Rd.GetChar(rd);       (* skip ":" *)
      server.port := Lex.Int(rd);
    EXCEPT
      Lex.Error, Rd.EndOfFile =>
        RAISE Error("Lex error scanning proxy host");
    END;
    RETURN server
  END Setup;


PROCEDURE Get (url: TEXT;
               VAR header: Header;
               READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
               forceCache: BOOLEAN := FALSE;
               server: T := NIL): Rd.T RAISES {Error, Thread.Alerted, IP.Error}
 =
  VAR
    channel: TCP.T;
    rd     : Rd.T;
  <* FATAL OSError.E, Rd.EndOfFile, Rd.Failure,  Wr.Failure,
     ConnFD.TimedOut *>
  BEGIN
    IF server = NIL THEN
      IF DefaultServer = NIL THEN DefaultServer := Setup() END;
      server := DefaultServer
    END;
    channel := OpenTCPConnect(server);
    ChannelPut(channel, Fmt.F("GET %s HTTP/1.0\n\r", url));
    FOR i := 0 TO LAST(requestFields) DO
      ChannelPut(channel, Fmt.F("%s\n\r", requestFields[i]));
    END;
    IF forceCache THEN
      ChannelPut(channel, "Pragma: no-cache\n\r");
    END;
    ChannelPut(channel, "\n\r");
    rd := NEW(TCPRd, channel := channel).init(
            FilePosix.New(channel.fd, FilePosix.Read));
    header := ParseHead(rd);
    RETURN rd;
  END Get;

PROCEDURE GetHead(url: TEXT;
               READONLY requestFields: ARRAY OF TEXT := DefaultRequestFields;
               forceCache: BOOLEAN := FALSE;
               server: T := NIL): Rd.T RAISES {Error, Thread.Alerted, IP.Error}
 =
  VAR
    channel: TCP.T;
    rd     : Rd.T;
  <* FATAL OSError.E, Rd.EndOfFile, Rd.Failure,  Wr.Failure,
     ConnFD.TimedOut *>
  BEGIN
    IF server = NIL THEN
      IF DefaultServer = NIL THEN DefaultServer := Setup() END;
      server := DefaultServer
    END;
    channel := OpenTCPConnect(server);
    ChannelPut(channel, Fmt.F("HEAD %s HTTP/1.0\n\r", url));
    FOR i := 0 TO LAST(requestFields) DO
      ChannelPut(channel, Fmt.F("%s\n\r", requestFields[i]));
    END;
    IF forceCache THEN
      ChannelPut(channel, "Pragma: no-cache\n\r");
    END;
    ChannelPut(channel, "\n\r");
    rd := NEW(TCPRd, channel := channel).init(
            FilePosix.New(channel.fd, FilePosix.Read));
    RETURN rd;
  END GetHead;

PROCEDURE AbsoluteURL (url: TEXT; base: TEXT): TEXT =
  VAR i: INTEGER;
  BEGIN
    IF url = NIL THEN
      RETURN NIL

    ELSIF Text.GetChar(url, 0) = '/' THEN
      (* Relative to base's host. *)
      i := Text.FindChar(base, ':');
      i := Text.FindChar(base, '/', i + 3);
      url := Text.Sub(base, 0, i) & url;

    ELSIF Text.FindChar(url, ':', 0) # -1 THEN
      (* Absolute form already. *)

    ELSE
      (* Relative to base's host and directory. *)
      i := Text.FindCharR(base, '/');
      url := Text.Sub(base, 0, i) & "/" & url;
    END;

    (* Add trailing slash if need be. *)
    i := Text.FindChar(url, ':', 0);
    IF Text.FindChar(url, '/', i + 3) = -1 THEN
      url := url & "/";
    END;

    RETURN url;
  END AbsoluteURL;

PROCEDURE ParseHead(rd: Rd.T): Header 
  RAISES {Error, Thread.Alerted, IP.Error} =
  VAR
    result: Header;
  BEGIN
    (* ugly header parsing code deleted *)
    RETURN result;
  END ParseHead;

BEGIN
END Web.

(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 27 13:29:29 PDT 1996 by najork                   *)
(*      modified on Tue Nov 14 02:48:25 PST 1995 by mhb                      *)

MODULE SimpleWeb;

IMPORT CIText, Fmt, IP, Rd, TextWr, Wr, RdCopy, RdUtils, 
       Thread, URLCache, Web, Text, Pathname, IO;

CONST
  MIMETypeAsText = ARRAY Web.MIMEType OF
                     TEXT{"application", "audio", "image", "message",
                          "multipart", "text", "video", "X-????"};

VAR
  extensionTypes := ARRAY [0..8] OF ExtensionType{
      ExtensionType{"gif",Web.MIMEType.Image,"gif"},
      ExtensionType{"jpeg",Web.MIMEType.Image,"jpeg"},
      ExtensionType{"pnm",Web.MIMEType.Image,"pnm"},
      ExtensionType{"ppm",Web.MIMEType.Image,"ppm"},
      ExtensionType{"pbm",Web.MIMEType.Image,"pbm"},
      ExtensionType{"pgm",Web.MIMEType.Image,"pgm"},
      ExtensionType{"htm",Web.MIMEType.Text,"html"},
      ExtensionType{"html",Web.MIMEType.Text,"html"},
      ExtensionType{"txt",Web.MIMEType.Text,"plain"}};

PROCEDURE Setup(READONLY a: ARRAY OF ExtensionType) =
  BEGIN
    extensionTypes := a;
  END Setup;

PROCEDURE Fetch (         url    : TEXT;
                 READONLY accepts: ARRAY OF TEXT := DefaultAccepts;
                          reload : BOOLEAN       := FALSE;
                          server : Web.T         := NIL             ):
  Web.Page RAISES {Thread.Alerted} =
  BEGIN
    RETURN DoFetch(url, accepts, reload, server, 1);
  END Fetch;

PROCEDURE DoFetch (         url    : TEXT;
                   READONLY accepts: ARRAY OF TEXT := DefaultAccepts;
                            reload : BOOLEAN;
                            server : Web.T;
                            linkCt : INTEGER                          ):
  Web.Page RAISES {Thread.Alerted} =
  <* FATAL Wr.Failure *>
  VAR
    header  : Web.Header;
    contents: TEXT;
    rd      : Rd.T;
    wr                   := TextWr.New();

  PROCEDURE Error (msg: TEXT) =
    VAR errorHeader: Web.Header;
    BEGIN
      contents := "** error fetching url '" & url & "': " & msg;
      header := errorHeader;
      header.httpVersion := "";
      header.statusCode := 0;
      header.reason := "";
      header.contentType := Web.MIMEType.Text;
      header.contentSubType := "plain";
    END Error;


  BEGIN
    IF reload OR NOT URLCache.Get (url, header, contents) THEN
      TRY
        TRY
          IF Text.Length(url) >= 5 AND 
             Text.Equal("http:",Text.Sub(url,0,6)) THEN
            rd := Web.Get(url, header, forceCache := reload, server := server);
          ELSIF Text.Length(url) >= 5 AND 
             Text.Equal("file:",Text.Sub(url,0,5)) THEN
            rd := FileGet(Text.Sub(url,5), header, forceCache := reload, 
                server := server);
          ELSIF Text.Length(url) >= 1 AND Text.GetChar(url,0) = '/' THEN
            rd := FileGet(url, header, forceCache := reload, server := server);
          ELSE
            rd := Web.Get(url, header, forceCache := reload, server := server);
          END;

          IF (header.statusCode = 301 OR header.statusCode = 302)
               AND header.location # NIL THEN
            IF linkCt > 5 THEN
              Error("url has moved and moved and ...")
            ELSE
              RETURN DoFetch(header.location, accepts, reload, server,
                             linkCt + 1)
            END
          ELSIF header.statusCode >= 300 THEN
            Error(header.reason)
          ELSIF NOT Acceptable(header, accepts) THEN
            Error("cannot handle content type '"
                    & MIMETypeAsText[header.contentType] & "/"
                    & header.contentSubType & "'")
          ELSE
            EVAL RdCopy.ToWriter(rd, wr);
            contents := TextWr.ToText(wr);
            URLCache.Put (url, header, contents)
          END;
        EXCEPT
        | Web.Error (msg) => Error(msg)
        | IP.Error => Error("IP error; probably cannot connect to host")
        | Rd.Failure (code) =>
            Error(Fmt.F("reader failure: %s\n", RdUtils.FailureText(code)))
        END;
      FINALLY
        Wr.Close(wr);
        IF rd # NIL THEN
          TRY
            Rd.Close(rd)
          EXCEPT
            Rd.Failure (code) =>
              Error(Fmt.F("reader failure closing connection: %s\n",
                          RdUtils.FailureText(code)))
          END
        END
      END
    END;

    IF header.location = NIL THEN 
      (* if the document moved and we have called DoFetch recursively to find it,
         the server doesn't always fill in the Location field. do this explicitly
         so clients will know the real URL *)
      header.location := url 
    END;

    RETURN NEW(Web.Page, header := header, contents := contents);
  END DoFetch;

PROCEDURE FileGet(    url          : TEXT; 
             VAR      header       : Web.Header;
  <*UNUSED*> READONLY requestFields: ARRAY OF TEXT := Web.DefaultRequestFields;
  <*UNUSED*>          forceCache   : BOOLEAN       := FALSE; 
  <*UNUSED*>          debug        : BOOLEAN       := FALSE; 
  <*UNUSED*>          server       : Web.T         := NIL): Rd.T 
    RAISES {Web.Error} =
  VAR
    rd := IO.OpenRead(url);
    type: Web.MIMEType := Web.MIMEType.Text;
    subType := "plain";
  BEGIN
    IF rd = NIL THEN RAISE Web.Error("Cannot open file " & url); END;
    GetType(url,type,subType);
    header.httpVersion := "";
    header.statusCode := 0;
    header.reason := "";
    header.contentType := type;
    header.contentSubType := subType;
    RETURN rd;
  END FileGet;

PROCEDURE GetType(url: TEXT; VAR type: Web.MIMEType; VAR subType: TEXT) =
  VAR
    ext := Pathname.LastExt(url);
  BEGIN
    FOR i := 0 TO LAST(extensionTypes) DO
      IF Text.Equal(ext,extensionTypes[i].ext) THEN
        type := extensionTypes[i].type;
        subType := extensionTypes[i].subType;
      END;
    END;
  END GetType;

PROCEDURE Acceptable (READONLY header : Web.Header;
                      READONLY accepts: ARRAY OF TEXT): BOOLEAN =
  VAR
    t := MIMETypeAsText[header.contentType] & "/" & header.contentSubType;
  BEGIN
    FOR i := FIRST(accepts) TO LAST(accepts) DO
      IF CIText.Equal(t, accepts[i]) THEN RETURN TRUE END
    END;
    RETURN FALSE
  END Acceptable;


BEGIN
END SimpleWeb.


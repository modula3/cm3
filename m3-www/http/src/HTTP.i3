(* Copyright (C) 1995, Digital Equipment Corporation. *)
(* All rights reserved. *)
(* Last modified on Sat Feb 22 15:13:48 PST 1997 by steveg *)

INTERFACE HTTP;

(* This interface covers HTTP version 0.9, 1.0 and 1.1.  Preference is
   given to HTTP 1.1 as the primary version.

   Correct behaviour is defined by the HTTP 1.1 and HTTP 1.0 specifications.
 *)

IMPORT
  App, Rd, Thread, Time, Wr;

TYPE
  Version = RECORD major, minor: INTEGER; END;

CONST
  Version0_9 = Version{0, 9}; 
  Version1_0 = Version{1, 0};
  Version1_1 = Version{1, 1};

CONST
  CurrentVersion = Version1_1;
  (* The default version of HTTP for this interface *)

(* This interface uses a canonical format for HTTP URL as follows:
   the scheme is in lower-case
   the host name is in lower-case
   when reading a URL, all escaped character (%xx) are converted
   to their ASCII equivalent (except for the "query" field 
   when writing a URL, all "reserved" and "unsafe" characters are converted
   to their escaped (lower-case hex) representation
 
   The first of these URL's is the canonical written representation.  All four
   URL's are equivalent.

      http://abc.com:/%7esmith/home.html
      http://abc.com:80/~smith/home.html
      http://ABC.com/%7Esmith/home.html
      http://ABC.com:/%7esmith/home.html
  *)

TYPE
  Style = REF RECORD
    version: Version;
    wrappedLines, absoluteURLs: BOOLEAN;
    (* "wrappedLines" = TRUE means that field values can wrap to
         the next line if it begins with white space
       "absoluteURLs" = TRUE means that all generated URLs should
         be absolute (include the host name) and not be relative *)
    viaFieldValue: TEXT;
  END;

PROCEDURE SetDefaultViaFieldValue(READONLY version: Version;
                                   port   : INTEGER;
                                   alias  : TEXT      := NIL);
  (* generate and set the default viaFieldValue for the default style.
     This field MUST be set for proxies.  The default value has
     the form:

     <hostname> <version> <programName>

     If "alias" is non-NIL, then it is used for "hostName" otherwise
     it will be host:port.  programName comes from the programInfo and
     so "SetDefaultViaFieldValue" SHOULD be called after
     "SetProgramInfo"
  *)

PROCEDURE DefaultStyle(READONLY version: Version := CurrentVersion): Style;
(* allocates and returns the default style for the given version, or
   the default style for the closest version if the version is not known *)

TYPE
  URLFormat = {Default, Canonical, BodyOnly};

TYPE
  URL <: URLPublic;
  URLPublic =
    OBJECT
      absPath                      : BOOLEAN := TRUE;
      scheme                       : TEXT    := "http";
      host                         : TEXT    := "";
      port                         : INTEGER := 80;
      path, params, query, fragment: TEXT    := "";
      (* URL format is:
         [<scheme>:][//<host>[:port]][[/]path][;params][?query][#fragment]
         The values of the parsed fields are all in canonical form.
         "absPath" is TRUE if the '/' exists before "path".  All field
         except for "query" have been unescaped - it can't be bacause it
         has a format that depends on escapable characters *)
      url: TEXT;
      (* "url" is the original text source of the URL (if known).  The
         "toText" method returns this field (if it exists) it is important
         to set "url" to NIL if ANY field is changed. *)
    METHODS
      init       (url: TEXT; log: App.Log): URL RAISES {App.Error};
      initFromRd (rd: Rd.T; log: App.Log): URL RAISES {App.Error};
      (* parses a url into its components in canonical format "relative" is
         true if only the body of the URL is defined.

         If the scheme, host, body or fragment is empty, the empty string
         is used.  If the port is not given, then "ParseURL" assigns the
         normal default port for the scheme (-1 if no scheme is given). *)
      toText (format := URLFormat.Default): TEXT;
      (* URL's can be displayed in several formats.  The "Default" format
         is the value of the "url" field or the "Canonical" format if the
         "url" field is NIL.  The "Canonical" format is the assemblage of
         the url's fields in canonical format.  The "BodyOnly" format skips
         the "host" and "port" fields.  (It is used for "old-style" - pre
         1.1 - GETs) *)
      equivalent (url: URL): BOOLEAN;
      (* "equivalent" returns TRUE if "self" and "url" are equal in
         canonical format *)
      local (service: INTEGER): BOOLEAN;
      (* "local" returns TRUE if the URL is local to the server associated
         with the service numberer "service"*)
      derelativize (url: URL): URL;
      (* if "self" is a relative URL, then "derelativize" returns the
         absolute URL produced by making self relative to "url".  If "self"
         is an absolute URL, then "self" is returned. *)
    END;

TYPE
  Field <: FieldPublic;
  FieldPublic = OBJECT
                  name, value: TEXT;
                METHODS
                  init (name, value: TEXT): Field;
                END;

  Header <: HeaderPublic;
  (* An HTTP header includes a series of name/value pairs. *)
  HeaderPublic =
    OBJECT
    METHODS
      lookupField (name: TEXT; value: TEXT := NIL): Field;
      (* return the value of the field named "name", or NIL if no field
         with "name" exists.  If "value" is NIL, then the first field with
         name is returned, otherwise, the first field with that name and
         value is returned. *)
      addField (field: Field; after: Field := NIL): Field;
      (* add a field with "name" and "value" to the header.  There can be
         multiple fields with the same name.  If "after" is NIL then
         "field" will be added as the first field, otherwise "field" will
         be added after "after".  If "after" is not in the list, then
         "field" will be added as the first field.  The result is the new
         field. *)
      removeField (field: Field): BOOLEAN;
      (* remove a field with name "field.name".  If "field.value" is NIL,
         then the first field with the name is removed.  Otherwise, the
         first field with a matching name and value is removed and TRUE is
         returned.  If no field matches, no change is made and FALSE is
         returned. *)
      copyFields (to: Header);
      (* add all of the field to "to" *)
      iterateFields (): FieldIterator;
      (* returns an iterator for all of the fields in the header *)
    END;

  FieldIterator <: FieldIteratorPublic;
  FieldIteratorPublic =
    OBJECT
    METHODS
      next (): Field;
      (* returns the next field, returns NIL after last field.  If the
         field list is modified, while it is iterated, then the result of
         "next" is unspecified. *)
    END;

TYPE
  FieldType =
    {Accept, Accept_Charset, Accept_Encoding, Accept_Language,
     Accept_Ranges, Age, Allow, Authorization, Cache_Control, Connection,
     Content_Base, Content_Encoding, Content_Language, Content_Length,
     Content_Location, Content_MD5, Content_Range, Content_Type, Date,
     ETag, Expires, From, Host, If_Modified_Since, If_Match, If_None_Match,
     If_Range, If_Unmodified_Since, Last_Modified, Location, Max_Forwards,
     Pragma, Proxy_Authenticate, Proxy_Authorization, Public, Range,
     Referer, Retry_After, Server, Transfer_Encoding, Upgrade, User_Agent,
     Vary, Via, Warning, WWW_Authenticate};

CONST
  FieldName = ARRAY FieldType OF
                     TEXT{
                     "Accept", "Accept-Charset", "Accept-Encoding",
                     "Accept-Language", "Accept-Ranges", "Age", "Allow",
                     "Authorization", "Cache-Control", "Connection",
                     "Content-Base", "Content-Encoding",
                     "Content-Language", "Content-Length",
                     "Content-Location", "Content-MD5", "Content-Range",
                     "Content-Type", "Date", "ETag", "Expires", "From",
                     "Host", "If-Modified-Since", "If-Match",
                     "If-None-Match", "If-Range", "If-Unmodified-Since",
                     "Last-Modified", "Location", "Max-Forwards", "Pragma",
                     "Proxy-Authenticate", "Proxy-Authorization", "Public",
                     "Range", "Referer", "Retry-After", "Server",
                     "Transfer-Encoding", "Upgrade", "User-Agent", "Vary",
                     "Via", "Warning", "WWW-Authenticate"};

TYPE
  StatusType =
    {Continue, Switching_Protocols, OK, Created, Accepted,
     Non_Authoritative_Information, No_Content, Reset_Content,
     Partial_Content, Multiple_Choices, Moved_Permanently,
     Moved_Temporarily, See_Other, Not_Modified, Use_Proxy, Bad_Request,
     Unauthorized, Payment_Required, Forbidden, Not_Found,
     Method_Not_Allowed, Not_Acceptable, Proxy_Authentication_Required,
     Request_Time_out, Conflict, Gone, Length_Required,
     Precondition_Failed, Request_Entity_Too_Large, Request_URI_Too_Large,
     Unsupported_Media_Type, Internal_Server_Error, Not_Implemented,
     Bad_Gateway, Service_Unavailable, Gateway_Time_out,
     HTTP_Version_not_supported};

CONST
  StatusCode = ARRAY StatusType OF
                   INTEGER{
                   100, 101, 200, 201, 202, 203, 204, 205, 206, 300, 301,
                   302, 303, 304, 305, 400, 401, 402, 403, 404, 405, 406,
                   407, 408, 409, 410, 411, 412, 413, 414, 415, 500, 501,
                   502, 503, 504, 505};
  StatusReason = ARRAY StatusType OF
                 TEXT{
                 "Continue", "Switching Protocols", "OK", "Created",
                 "Accepted", "Non-Authoritative Information", "No Content",
                 "Reset Content", "Partial Content", "Multiple Choices",
                 "Moved Permanently", "Moved Temporarily", "See Other",
                 "Not Modified", "Use Proxy", "Bad Request",
                 "Unauthorized", "Payment Required", "Forbidden",
                 "Not Found", "Method Not Allowed", "Not Acceptable",
                 "Proxy Authentication Required", "Request Time-out",
                 "Conflict", "Gone", "Length Required",
                 "Precondition Failed", "Request Entity Too Large",
                 "Request-URI Too Large", "Unsupported Media Type",
                 "Internal Server Error", "Not Implemented", "Bad Gateway",
                 "Service Unavailable", "Gateway Time-out",
                 "HTTP Version not supported"};

TYPE
  Method = {Options, Get, Post, Put, Delete, Head, Trace, Connect};

CONST
  MethodText = ARRAY Method OF TEXT{"OPTIONS", "GET", "POST", "PUT",
                                    "DELETE", "HEAD", "TRACE", "CONNECT"};

TYPE
  Request <: RequestPublic;
  RequestPublic =
    Header OBJECT
      method  : Method;
      url     : URL;
      version : Version  := CurrentVersion;
      postData: TEXT     := NIL;
    METHODS
      parse (rd: Rd.T; log: App.Log): Request RAISES {App.Error};
      (* Parse the HTTP request in "rd", sends any messages to "log", and
         returns the parsed header of the request.  On return, "rd" points
         to the first character after the header. *)
      write (wr: Wr.T; style: Style; proxyRequest: BOOLEAN; log: App.Log)
             RAISES {App.Error};
      (* Write the HTTP request described in "request" to "wr".  If
         "proxyRequest" then the full URL is written, otherwise only the
         body is writen.  If "style" is NIL then DefaultStyle() is used. *)
      toText (style: Style; proxyRequest: BOOLEAN; log: App.Log): TEXT
              RAISES {App.Error};
      (* return a text of the request.  If "proxyRequest" then the full URL
         is written, otherwise only the body is writen.  If "style" is NIL
         then DefaultStyle() is used. *)
    END;

TYPE
  Reply <: ReplyPublic;
  ReplyPublic =
    Header OBJECT
      version: Version := CurrentVersion;
      code   : INTEGER := StatusCode[StatusType.OK];
      reason : TEXT    := StatusReason[StatusType.OK];
    METHODS
      parse (rd: Rd.T; log: App.Log): Reply RAISES {App.Error};
      (* Parse the HTTP reply in "rd", sends any messages to "log", and
         returns the parsed header of the reply.  On return, "rd" points to
         the first character after the header.

         If the reply is an HTTP/0.9 reply, then "self.version" is
         Version0_9 and "self.reason" is the text already read while
         looking for the first HTTP line. *)
      write (wr: Wr.T; style: Style; log: App.Log) RAISES {App.Error};
      (* Write the HTTP Reply headers to "wr".  If "style" is NIL, then
         DefaultStyle() is used. *)

      toText (style: Style; log: App.Log): TEXT RAISES {App.Error};
      (* return a text of the reply.  If "style" is NIL, then
         DefaultStyle() is used. *)
    END;

PROCEDURE WriteSimpleReplyHeader (wr     : Wr.T;
                                  style  : Style;
                                  log    : App.Log;
                                  code: INTEGER := StatusCode[
                                                     StatusType.OK];
                                  reason: TEXT := StatusReason[
                                                    StatusType.OK])
  RAISES {App.Error};
  (* write the first line of a HTTP reply to "wr" based on 
     "style", "code" and "reason".  The client can follow
     this with additional headers and must write a blank line
     to end the header.

     If "style" is NIL, then DefaultStyle() is used.

  *)

PROCEDURE WriteRedirectReply(wr: Wr.T; url, htmlMsg: TEXT; log: App.Log) 
  RAISES {App.Error};
  (* write a redirect to "url" with content of "htmlMsg" reply to "wr".
     IF "htmlMsg" is NIL then a generic resource has moved message is
     given.  *)

PROCEDURE WriteTime(wr: Wr.T; time: Time.T; log: App.Log) RAISES {App.Error};
PROCEDURE ReadTime(rd: Rd.T; log: App.Log): Time.T RAISES {App.Error};
(* Write the time in RFC 822/RFC 1123 format.
   Read the time in RFC 822/RFC 1123, RFC 850, asctime, and other formats *)

TYPE
  ProgramType = {Client, Proxy, Server, Tunnel};
  ProgramInfo = RECORD
                  type                  : ProgramType;
                  name                  : TEXT;
                  authType                              := AuthType.None;
                  authRealm, authAccount: TEXT          := "";
                END;
  (* if "authType" is not "AuthType.None" then "authRealm" and "authAccount"
     are used for authentication *)

PROCEDURE SetProgramInfo(READONLY programInfo: ProgramInfo);
PROCEDURE GetProgramInfo(): ProgramInfo;
(* Get and set the program information.  Depending on the type of the
   program, the appropriate field(s) is (are) added to the requests
   and response.  If "programType" is "Proxy" then the program should
   call "SetDefaultViaFieldValue".  *)

EXCEPTION
  BadFormQuery;

TYPE
  FormQuery <: FormQueryPublic;
  FormQueryPublic =
    Header OBJECT
    METHODS
      init (query: TEXT): FormQuery RAISES {BadFormQuery};
      initFromRd (rd: Rd.T): FormQuery
                    RAISES {BadFormQuery};
      write  (wr: Wr.T; log: App.Log) RAISES {App.Error};
      toText (): TEXT;
    END;
  (* a FormQuery corresponds to a parsed query segment with the format:
      name=value&name=value&name=value

     each name=value pair becomes a field of the form object.

     the %xx encoded characters ARE unescaped when the form query is
     initialized, and they ARE escaped when the form query is written.
   *)

TYPE
  AuthType = {None, Server, Proxy};

PROCEDURE BasicAuthField(account: TEXT; auth: AuthType): Field;
  (* Returns the Basic authentication field for "account" (name:password)
     for either a server or proxy.
     Basic authentication is described in:
       http://www.w3.org/pub/WWW/Protocols/HTTP1.0/draft-ietf-http-spec.html#BasicAA
  *)

PROCEDURE AuthorizedRequest (request: Request;
                             auth   : AuthType;
                             account: TEXT;
                             log    : App.Log   ): BOOLEAN
  RAISES {App.Error};
  (* Returns TRUE if "request" has valid authentication for
     "account" (formatted as: "name:password") on either the server or proxy *)

PROCEDURE ReplyUnauthorized (wr        : Wr.T;
                             auth      : AuthType;
                             realm     : TEXT;
                             log       : App.Log;
                             defaultMsg: BOOLEAN    := TRUE)
  RAISES {App.Error};
  (* Write an "unauthorized" reply to "wr" covering "realm" for 
     either the server or proxy.  Send "Content-type: text/html".
     If "defaultMsg" then a simple
     default message is given.  Otherwise the client is responsible for
     providing the message. *)

CONST
  Base64Decode = ARRAY CHAR OF INTEGER{
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63, (* +, / *)
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1, (* 0..9 *)
    -1, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, (* A..O *)
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, (* P..Z *)
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, (* a..o *)
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, ..};            (* p..z *)


  Base64Encode = ARRAY [0..63] OF CHAR{
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 
    'w', 'x', 'y', 'z', '0', '1', '2', '3', 
    '4', '5', '6', '7', '8', '9', '+', '/'};

(* Decode and return the <user>:<password> authorization field in a header,
   or NIL if not there *)
PROCEDURE AuthorizationAccount (request: Request;
                                auth   : AuthType;
                                log    : App.Log   ): TEXT
  RAISES {App.Error};

EXCEPTION CopyError;

TYPE
  Dest = OBJECT
  METHODS
    copy(READONLY a: ARRAY OF CHAR)
      RAISES {Wr.Failure, Thread.Alerted, CopyError};
  END;
  (* "copy" processes the characters in "a".  "a" may be of 
     any length.  The number of times "copy" is called and the
     total characters in the calls is not known ahead of time. *)

  WrDest <: WrDestPublic;
  WrDestPublic = Dest OBJECT
  METHODS
    init(wr: Wr.T): WrDest;
    (* A "WrDest.copy" puts its data into "wr" *)
  END;
  
TYPE
  Src = OBJECT
  METHODS
    fill(VAR (* OUT *) a: ARRAY OF CHAR): CARDINAL 
        RAISES {Rd.Failure, Thread.Alerted, CopyError};
  END;
  (* "fill" puts characters in "a" and returns the number put in "a".
     If "fill" puts fewer than NUMBER(a) characters in "a" then
     it is assumed that there are no more characters and "fill" should
     not be called any more *)

  RdSrc <: RdSrcPublic;
  RdSrcPublic = Src OBJECT
  METHODS
    init(rd: Rd.T): RdSrc;
    (* A "RdSrc.fill" gets its data from "rd" *)
  END;
  
PROCEDURE ReadBody (requestOrReply: Header;
                    rd            : Rd.T;
                    dest          : Dest;
                    log           : App.Log ) RAISES {App.Error};
(* read the body from "rd" calling "dest.copy" as necessary.  The
   body is read using the transfer coding specifed in the "requestOrReply"
   header fields, content-length, chunked, closing the connection or 
   whatever. *)

PROCEDURE WriteBody (requestOrReply: Header;
                     wr            : Wr.T;
                     src           : Src;
                     log           : App.Log ) RAISES {App.Error};
(* write the body to "wr" calling "src.fill" as necessary.

   IF there is a "Transfer-Encoding: chunked" header field in
   "requestOrReply" then the body will be written in the chunked format.
   The end of the body is signified when "src.fill" runs out of characters
   (returns fewer than possible on some call).

   If there is a "Content_Length: <length>" header field in the
   "requestOrReply" header, then "<length>" bytes will be written.  It is
   an error if there are not "<length>" bytes available.

   Otherwise, the body is written and the end of the body is signified when
   "src.fill" runs out of characters (returns fewer than possible on some
   call). *)

PROCEDURE EscapeURLEntry(entry: TEXT): TEXT;
PROCEDURE UnescapeURLEntry(entry: TEXT; log: App.Log): TEXT RAISES {App.Error};
(* Escape or Unescape the characters in a URL body.  Reserved characters
   are escaped as %xx where xx is the hex code for the character. *)

PROCEDURE EncodeTextForHTML(text: TEXT): TEXT;
PROCEDURE DecodeTextForHTML(text: TEXT; log: App.Log): TEXT RAISES {App.Error};
(* Encode and Decode special HTML characters ("<>&) for display in HTML form fields *)

CONST
  Ctl = SET OF CHAR{'\000'.. '\037', '\177'};
  TSpecial = SET OF
               CHAR{'(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/',
                    '[', ']', '?', '=', '{', '}', ' ', '\t'};
  Token = SET OF CHAR {'\000' .. '\377'} - Ctl - TSpecial;

TYPE
  UserAgent = {Netscape, InternetExplorer, Other};

CONST
  NoVersion = -1;

(* Get the user agent and its version from the request. *)
PROCEDURE GetUserAgent (req: Request; VAR (* out *) version: INTEGER):
  UserAgent;

END HTTP.

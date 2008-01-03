(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan 18 11:18:19 PST 1996 by najork                   *)
(*      modified on Thu Jan 11 11:55:32 PST 1996 by mhb                      *)
<* PRAGMA LL *>

(* A cache of URL information. *)

MODULE URLCache;

IMPORT HTML, TextRefTbl, Web;


TYPE    
  Info = REF RECORD
    header: Web.Header;
    contents: TEXT;
  END;

VAR 
  urlCacheMu := NEW(MUTEX);
  urlCacheEnabled := TRUE;
  urlCache := NEW(TextRefTbl.Default).init();

PROCEDURE Get (url: TEXT; VAR header: Web.Header; VAR contents: TEXT):
  BOOLEAN =
  VAR
    ref  : REFANY;
    found         := FALSE;
  BEGIN
    LOCK urlCacheMu DO
      IF urlCacheEnabled THEN
        found := urlCache.get(url, ref);
        IF found THEN
          header := NARROW(ref, Info).header;
          contents := NARROW(ref, Info).contents
        END
      END
    END;
    RETURN found
  END Get;

PROCEDURE Put (url: TEXT; READONLY header: Web.Header; contents: TEXT) =
  BEGIN
    LOCK urlCacheMu DO
      IF urlCacheEnabled THEN
        VAR info := NEW(Info, header := header, contents := contents);
        BEGIN
          EVAL urlCache.put(url, info)
        END
      END
    END
  END Put;

PROCEDURE Enable () =
  BEGIN
    LOCK urlCacheMu DO urlCacheEnabled := TRUE END
  END Enable;

PROCEDURE Disable() =
  BEGIN
    LOCK urlCacheMu DO urlCacheEnabled := FALSE END
  END Disable;

PROCEDURE Flush () =
  BEGIN
    LOCK urlCacheMu DO urlCache := NEW(TextRefTbl.Default).init() END
  END Flush;


VAR 
  htmlCacheMu := NEW(MUTEX);
  htmlCacheEnabled := TRUE;
  htmlCache := NEW(TextRefTbl.Default).init();

PROCEDURE GetHTML (url: TEXT; VAR html: HTML.T): BOOLEAN =
  VAR
    ref  : REFANY;
    found: BOOLEAN := FALSE;
  BEGIN
    LOCK htmlCacheMu DO
      IF htmlCacheEnabled THEN
        found := htmlCache.get(url, ref);
        IF found THEN html := NARROW(ref, HTML.T) END
      END
    END;
    RETURN found
  END GetHTML;

PROCEDURE PutHTML (url: TEXT; html: HTML.T) =
  BEGIN
    LOCK htmlCacheMu DO
      IF htmlCacheEnabled THEN EVAL htmlCache.put(url, html) END
    END
  END PutHTML;

PROCEDURE EnableHTML () =
  BEGIN
    LOCK htmlCacheMu DO htmlCacheEnabled := TRUE END
  END EnableHTML;

PROCEDURE DisableHTML() =
  BEGIN
    LOCK htmlCacheMu DO htmlCacheEnabled := FALSE END
  END DisableHTML;

PROCEDURE FlushHTML () =
  BEGIN
    LOCK htmlCacheMu DO htmlCache := NEW(TextRefTbl.Default).init() END
  END FlushHTML;

BEGIN
END URLCache.



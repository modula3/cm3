(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Stephen Harrison *)
(* *)
(* Last modified on Tue Jul 21 20:28:21 PDT 1992 by harrison *)

MODULE CirclePixmapCache;

IMPORT CirclePixmap, Pixmap;

(* We maintain a cache of circles (ellipses).  There is only one pixmap
   held for each unique width, height, border width and fill mode
   combination.  The /cache/ field in a CirclePixmapCache.T is an array of
   previously-computed pixmaps.  We use an existing pixmap if we already
   have one of the right size, otherwise we create and remember a new one.
   Simple. *)

REVEAL
  T = T_Public BRANDED OBJECT
        cache: REF ARRAY OF CacheEntry := NIL;
        count := 0;
      OVERRIDES
        create := Create;
        purge := Purge;
      END;

TYPE
  Config = RECORD
             width, height, border: CARDINAL;
             fill: BOOLEAN;
           END;

  CacheEntry = RECORD
                 pixmap: Pixmap.T;
                 config: Config;
               END;

PROCEDURE New (remember := DEFAULT_REMEMBER): T =
  VAR r := MAX(1, remember);
  BEGIN
    RETURN NEW(T, cache := NEW(REF ARRAY OF CacheEntry, r));
  END New;

PROCEDURE CompareConfig (READONLY a, b: Config): [-1 .. +1] =
  BEGIN
    IF a.height < b.height THEN
      RETURN -1;
    ELSIF a.height > b.height THEN
      RETURN +1;

    ELSIF a.width < b.width THEN
      RETURN -1;
    ELSIF a.width > b.width THEN
      RETURN +1;

    ELSIF a.border < b.border THEN
      RETURN -1;
    ELSIF a.border > b.border THEN
      RETURN +1;

    ELSIF a.fill < b.fill THEN
      RETURN -1;
    ELSIF a.fill > b.fill THEN
      RETURN +1;
    END;

    RETURN 0;
  END CompareConfig;

PROCEDURE Create (self         : T;
                  width, height: CARDINAL;
                  border       : CARDINAL   := 0;
                  fill                      := TRUE): Pixmap.T =
  VAR
    low, mid, cond := 0;
    high := self.count - 1;
    config := Config{width, height, border, fill};
  BEGIN
    WHILE low <= high DO
      mid := (low + high) DIV 2;
      WITH test = self.cache[mid] DO
        cond := CompareConfig(config, test.config);
      END;
      IF cond < 0 THEN
        high := mid - 1;
      ELSIF cond > 0 THEN
        low := mid + 1;
      ELSE
        RETURN self.cache[mid].pixmap;
      END;
    END;

    (* An ellipse pixmap for these dimensions does not exist---create a new
       one. *)
    VAR insert_at := 0;
    BEGIN
      IF self.count >= NUMBER(self.cache^) THEN
        Purge(self);
      ELSE
        insert_at := self.count;
        FOR i := 0 TO self.count - 1 DO
          WITH test = self.cache[i],
               cond = CompareConfig(config, test.config) DO
            (* Table got unsorted---we're hosed. *)
            <* ASSERT cond # 0 *>
            IF cond < 0 THEN insert_at := i; EXIT; END;
          END;
        END;

        FOR i := self.count TO insert_at + 1 BY -1 DO
          self.cache[i] := self.cache[i - 1];
        END;
      END;

      self.cache[insert_at] :=
        CacheEntry{CirclePixmap.New(width, height, border, fill), config};
      INC(self.count);

      RETURN self.cache[insert_at].pixmap;
    END;
  END Create;

PROCEDURE Purge (self: T) =
  BEGIN
    self.count := 0;
  END Purge;

BEGIN
END CirclePixmapCache.


(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Wed Mar 22 18:17:10 PST 1995 by msm      *)
(*      modified on Tue Feb 22 23:09:30 PST 1994 by kalsow   *)
(*      modified on Mon Oct 25 12:19:23 PDT 1993 by sfreeman *)

(* note: SIGPIPE set to be ignored by JVSink *)

UNSAFE MODULE Jvs;

IMPORT Atom, AtomList, Ctypes, Jv, JvsProtocol, M3toC, OSError, Point,
       RTMisc, Text, Thread, Word;

CONST DefaultColors = 50;

REVEAL
  T = Public BRANDED OBJECT
        dparams                 := DefaultDecompress;
        cmap   : ColormapInfo;
      OVERRIDES
        init             := Init;
        allocateBuffer   := AllocateBuffer;
        deallocateBuffer := DeallocateBuffer;
        compress         := Compress;
        setCompress      := SetCompress;
        decompress       := Decompress;
        setDecompress    := SetDecompress;
        colormap         := Colormap;
        close            := Close;
      END;

PROCEDURE Init (t: T): T RAISES {OSError.E} =
  BEGIN
    TRY
      LOCK t DO RETURN Jv.T.init(t, JvsProtocol.PipeName); END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END Init;

PROCEDURE AllocateBuffer (t: T; type: BufferType; width, height: CARDINAL):
  ShmBufId RAISES {OSError.E, Thread.Alerted} =
  VAR
    req: JvsProtocol.AllocateReq;
    rep: JvsProtocol.AllocateRep;
  BEGIN
    req.requestCode := JvsProtocol.Allocate;
    TRY
      CASE type OF
      | BufferType.Compress =>
          IF width # 0 THEN
            req.requestCode := JvsProtocol.Allocate2
          END;
          req.type := JvsProtocol.JPEG;
          req.width := width;
          req.height := 0;
      | BufferType.Decompress =>
          req.type := JvsProtocol.Dithered;
          IF width = 0 AND height = 0 THEN
            req.width := 1280;
            req.height := 1024
          ELSE
            req.width := width;
            req.height := height
          END
      END;
      req.direction := JvsProtocol.Output;
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(rep), BYTESIZE(rep));
      END;
      IF rep.requestCode # JvsProtocol.Allocate THEN
        (* error code of some sort *)
        RETURN 0;
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
    RETURN rep.shmid;
  END AllocateBuffer;

PROCEDURE DeallocateBuffer (t: T; shmid: ShmBufId)
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req: JvsProtocol.DeallocateReq;
    rep: JvsProtocol.DeallocateRep;
  BEGIN
    TRY
      req.shmid := shmid;
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(rep), BYTESIZE(rep));
      END;
      IF rep.requestCode # req.requestCode THEN
        RAISE OSError.E(AtomList.List1(Atom.FromText("DeallocateBuffer")));
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END DeallocateBuffer;

PROCEDURE Compress (t: T; dest: ShmBufId): CARDINAL
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req: JvsProtocol.CompressReq;
    rep: JvsProtocol.CompressRep;
  BEGIN
    TRY
      req.shmid := dest;
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(rep), BYTESIZE(rep));
      END;
      IF rep.requestCode # req.requestCode OR rep.shmid # req.shmid THEN
        RAISE OSError.E(AtomList.List1(Atom.FromText("Compress")));
      END;
      RETURN rep.length;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END Compress;

PROCEDURE SetCompress (t: T; qfactor, xdec, ydec, frameskip: INTEGER):
  Point.T RAISES {OSError.E, Thread.Alerted} =
  VAR
    req: JvsProtocol.SetCompressReq;
    rep: JvsProtocol.SetCompressRep;
  BEGIN
    TRY
      req.qfactor := qfactor;
      req.xdec := xdec;
      req.ydec := ydec;
      req.frameskip := frameskip;
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(rep), BYTESIZE(rep));
      END;
      IF rep.requestCode # req.requestCode THEN
        RAISE OSError.E(AtomList.List1(Atom.FromText("SetCompress")));
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
    RETURN Point.T{rep.width, rep.height};
  END SetCompress;

PROCEDURE Decompress (t: T; src, dest: ShmBufId; srcByteLength: CARDINAL)
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req: JvsProtocol.DecompressReq;
    rep: JvsProtocol.DecompressRep;
  BEGIN
    TRY
      req.cshmid := src;
      req.dshmid := dest;
      req.length := srcByteLength;
      LOCK t DO
        Jv.Send(t, ADR(req), BYTESIZE(req));
        Jv.Recv(t, ADR(rep), BYTESIZE(rep));
      END;
      IF rep.requestCode # req.requestCode OR rep.cshmid # req.cshmid
           OR rep.dshmid # req.dshmid THEN
        RAISE OSError.E(AtomList.List1(DecompressFailure));
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
  END Decompress;

(* the stuff for aligning the X axis is taken from Lance's JVideo.c code.
   He can explain it to you... *)
CONST
  DesiredRounding = BYTESIZE(Word.T);

PROCEDURE SetDecompress (t: T; VAR params: DcmpParams): BOOLEAN
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req         : JvsProtocol.SetDecompressReq;
    rep         : JvsProtocol.SetDecompressRep;
    roundedWidth: CARDINAL;
  BEGIN
    TRY
      LOCK t DO
        IF params = t.dparams THEN RETURN FALSE; END;

        req.qfactor := params.qfactor;
        req.inX := params.inX;
        req.inY := params.inY;
        req.outX := params.reqX;
        req.outY := params.reqY;
        req.brightness := params.brightness;
        req.contrast := params.contrast;
        req.saturation := params.saturation;

        LOOP
          Jv.Send(t, ADR(req), BYTESIZE(req));
          Jv.Recv(t, ADR(rep), BYTESIZE(rep));
          IF rep.requestCode # req.requestCode THEN
            RAISE
              OSError.E(AtomList.List1(Atom.FromText("SetDecompress")));
          END;
          roundedWidth := (((rep.actualOutX + rep.linePadding + 3) DIV 4) * 4);
          IF (roundedWidth MOD DesiredRounding) = 0 THEN EXIT; END;
          DEC(req.outX);
        END;

	linePadding := rep.linePadding;
        params.outX := roundedWidth;
        params.outY := rep.actualOutY;
        t.dparams := params;
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
    RETURN TRUE;
  END SetDecompress;

CONST BoolToInt = ARRAY BOOLEAN OF Ctypes.int{0, 1};

PROCEDURE Colormap (t: T; VAR info: ColormapInfo): BOOLEAN
  RAISES {OSError.E, Thread.Alerted} =
  VAR
    req   : JvsProtocol.ColormapReq;
    rep   : JvsProtocol.ColormapRep;
    length: CARDINAL;
  BEGIN
    TRY
      LOCK t DO
        WITH cmap = t.cmap DO
          IF cmap.id = info.id AND
               ((info.nColors = 0) # (cmap.nColors = info.nColors))
               AND cmap.monochrome = info.monochrome THEN
            RETURN FALSE;
          END;

          IF info.displayName # NIL THEN
            length := Text.Length(info.displayName);
            IF length > JvsProtocol.MaxXServerNameLen - 1 THEN
              RAISE OSError.E(AtomList.List1(XNameTooLong));
            END;

            WITH string = M3toC.TtoS(info.displayName) DO
              RTMisc.Copy(string, ADR(req.serverName[0]), length + 1);
            END;
          ELSE
            RTMisc.Zero(ADR(req.serverName[0]), BYTESIZE(req.serverName));
          END;

          req.nColors := info.nColors;
          req.id := info.id;
          req.monochrome := BoolToInt[info.monochrome];
          
          LOOP
            Jv.Send(t, ADR(req), BYTESIZE(req));
            Jv.Recv(t, ADR(rep), BYTESIZE(rep));
            IF rep.requestCode # req.requestCode THEN
              RAISE OSError.E(AtomList.List1(Atom.FromText("Colormap")));
            END;
            IF rep.nColors # 0 OR req.nColors # 0 THEN EXIT END;
            req.nColors := DefaultColors
          END;

          info.nColors := rep.nColors;
          t.cmap := info;
        END;
      END;
    EXCEPT
    | OSError.E (e) => RAISE OSError.E(AtomList.Cons(Jv.ServerFailure, e));
    END;
    RETURN TRUE;
  END Colormap;

PROCEDURE Close (t: T) =
  BEGIN
    LOCK t DO Jv.T.close(t); END;
  END Close;

BEGIN
  XNameTooLong := Atom.FromText("Jvs XNameTooLong");
  DecompressFailure := Atom.FromText("Decompress");
END Jvs.

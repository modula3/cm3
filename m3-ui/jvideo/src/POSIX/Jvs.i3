(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Wed Mar 22 17:50:21 PST 1995 by msm      *)
(*      modified on Thu Oct 21 17:07:10 PDT 1993 by sfreeman *)

(* video client interface to the local J-Video server.  All the methods
   lock the object and block the thread until the server replies *)

INTERFACE Jvs;

IMPORT Atom, Ctypes, Jv, OSError, Point, Thread;

TYPE
  ShmBufId = Ctypes.int;
  BufferType = {Compress, Decompress};

(* parameters for decompression *)
TYPE
  DcmpParams = RECORD
                 qfactor   : CARDINAL;
                 inX, inY  : INTEGER;
                 brightness: CARDINAL;
                 contrast  : CARDINAL;
                 saturation: CARDINAL;
                 reqX, reqY: INTEGER;   (* what the client wants *)
                 outX, outY: INTEGER;   (* what the client gets *)
               END;
CONST DefaultDecompress = DcmpParams{0, 640, 480, 0, 0, 0, 0, 0, 0, 0};

TYPE
  Id = Ctypes.unsigned_long_int; (* same as X *)
  ColormapInfo = RECORD
                   id         : Id       := IdNone;
                   nColors    : CARDINAL := 0; (* in/out *)
                   monochrome            := FALSE;
                   displayName: TEXT     := NIL;
                 END;
CONST IdNone = 0;                (* same as X *)


VAR XNameTooLong, DecompressFailure: Atom.T;
(* may be part of OSError.E list *)

TYPE
  T <: Public;
  Public =
    Jv.T OBJECT
    METHODS
      (* all methods LL < self *)

      init (): T RAISES {OSError.E};
      (* initialise local state and establish a connection with the local
         server *)

      allocateBuffer (type: BufferType; width, height: CARDINAL := 0):
                      ShmBufId RAISES {OSError.E, Thread.Alerted};
      (* allocate a shared memory buffer in the server, return its kernel
         id; 0,0 height and width give the default size, whil non-zero
         allocates a buffer of that size (or length if height is zero). *)
      deallocateBuffer (shmid: ShmBufId)
                        RAISES {OSError.E, Thread.Alerted};
      (* deallocate a shared memory buffer in the server *)

      setCompress (qfactor, xdec, ydec, frameskip: INTEGER): Point.T
                   RAISES {OSError.E, Thread.Alerted};
      (* set parameters for the compression stream *)

      compress (dest: ShmBufId): CARDINAL
                RAISES {OSError.E, Thread.Alerted};
      (* tells the server to compress a video frame into the the shared
         memory buffer *)

      decompress (src, dest: ShmBufId; srcByteLength: CARDINAL)
                  RAISES {OSError.E, Thread.Alerted};
      (* tells the server to to decompress "src" into "dest".  The server
         will zero the compressed data block from the end of the compressed
         data to the next 512-byte boundary *)

      setDecompress (VAR params: DcmpParams): BOOLEAN
                     RAISES {OSError.E, Thread.Alerted};
      (* set parameters for the decompression stream.  Returns actual width
         and height set by server in params.outX and params.outY.  Returns
         TRUE if parameters different from previous settings, otherwise
         returns FALSE and is a no-op *)

      colormap (VAR info: ColormapInfo): BOOLEAN
                RAISES {OSError.E, Thread.Alerted};
      (* tells the server to acquire "ncolors" colour cells for the
         colormap "id" in the X server "displayName" and fill them with the
         colours for the image stream.  Returns number of colours acutally
         allocated in info.ncolors.  Returns TRUE if settings different
         from previous, otherwise returns FALSE and is a no-op. *)

      (* "close()" now LL < self *)
    END;

VAR linePadding: INTEGER := 0;

END Jvs.

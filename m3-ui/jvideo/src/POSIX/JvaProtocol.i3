(* Copyright (C) 1989, 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Fri Jan  7 14:56:08 PST 1994 by msm      *)
(*      modified on Thu Oct 21 16:05:44 PDT 1993 by sfreeman *)

INTERFACE JvaProtocol;

IMPORT Jva;

FROM Ctypes IMPORT int;

CONST
  Socket = 2520;                 (* the port number for the local server *)
  PipeName    = "/tmp/jvideo/argohear.2520";
  (* client and server communicate via this named pipe *)


  MAXREQUEST = 256;

  FirstRequest = Connect;
  Connect      = 1;
  Mute         = 2;
  Volume       = 3;
  Statistics   = 4;
  LastRequest  = Statistics;

TYPE TypeCode = BITS BITSIZE(int) FOR [FirstRequest .. LastRequest];

CONST
  MuteOff = 0;
  MuteOn  = 1;

TYPE MuteCode = BITS BITSIZE(int) FOR [MuteOff .. MuteOn];

TYPE
  ConnectReqRec =
    RECORD
      request       : TypeCode := Connect;
      hostnameLength: int;
      (* data for hostname goes here for 'hostnameLength' bytes *)
    END;
  ConnectReqPtr = UNTRACED REF ConnectReqRec;

TYPE
  ConnectReplyRec = RECORD
                      request: int;  (* to check correct request code *)
                    END;
  ConnectReplyPtr = UNTRACED REF ConnectReplyRec;

TYPE
  MuteReqRec = RECORD
                 request: TypeCode   := Mute;
                 mute   : MuteCode;
               END;
  MuteReqPtr = UNTRACED REF MuteReqRec;

TYPE
  MuteReplyRec = RECORD
                   request: int;  (* to check correct request code *)
                 END;
  MuteReplyPtr = UNTRACED REF MuteReplyRec;

TYPE
  VolumeReqRec = RECORD
                   request: TypeCode := Volume;
                   volume : int;
                 END;
  VolumeReqPtr = UNTRACED REF VolumeReqRec;

TYPE
  VolumeReplyRec = RECORD
                     request: int;  (* to check correct request code *)
                   END;
  VolumeReplyPtr = UNTRACED REF VolumeReplyRec;

TYPE
  StatisticsReqRec = RECORD request: TypeCode := Statistics;  END;
  StatisticsReqPtr = UNTRACED REF StatisticsReqRec;

TYPE
  StatisticsReplyRec = RECORD
                         request: int;  (* to check correct request code *)
                         statistics: Jva.Statistics;
                       END;
  StatisticsReplyPtr = UNTRACED REF StatisticsReplyRec;

CONST
  HdrSizes = ARRAY TypeCode OF
               CARDINAL{
               BYTESIZE(ConnectReplyRec), BYTESIZE(MuteReplyRec),
               BYTESIZE(VolumeReplyRec), BYTESIZE(StatisticsReplyRec)};

(* no message is shorter than an AnyHeader, so we can always read in the
   number of bytes in an AnyHeader *)
TYPE
  AnyHeader = RECORD type: int;  END;
  AnyHeaderPtr = UNTRACED REF AnyHeader;

CONST
  MaxHdrSize = MaxHdrBytes DIV BYTESIZE(CHAR);
  MaxHdrBytes = MAX(BYTESIZE(AnyHeader),
                    MAX(HdrSizes[Connect],
                        MAX(HdrSizes[Mute],
                            MAX(HdrSizes[Volume], HdrSizes[Statistics]))));

TYPE
  Header = ARRAY [0 .. MaxHdrSize - 1] OF CHAR;
  HeaderPtr = UNTRACED REF Header;

END JvaProtocol.

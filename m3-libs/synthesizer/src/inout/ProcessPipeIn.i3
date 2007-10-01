INTERFACE ProcessPipeIn;
(* author: thielema *)

FROM ProcessInOut IMPORT WordSize;
IMPORT Signal, ProcessMultiOutput;

IMPORT Rd, Thread;

PROCEDURE Do (         prog    : TEXT;
              READONLY params  : ARRAY OF TEXT;
                       wordSize                  := WordSize.Bits16; ):
  Signal.RefArray RAISES {Rd.Failure, Thread.Alerted};


TYPE
  T <: Public;
  Public =
    Signal.T OBJECT
    METHODS
      init (         prog    : TEXT;
            READONLY params  : ARRAY OF TEXT;
                     wordSize                  := WordSize.Bits16; ): T;
    END;

TYPE
  Multi <: MultiPublic;
  MultiPublic = ProcessMultiOutput.T OBJECT
                METHODS
                  init (         numChannels: CARDINAL;
                        READONLY channels   : ARRAY OF CARDINAL;
                                 prog       : TEXT;
                        READONLY params     : ARRAY OF TEXT;
                        wordSize := WordSize.Bits16; ): Multi;
                END;

END ProcessPipeIn.

(* $Id$ *)
INTERFACE FrameBufferOutput;
IMPORT FrameBuffer, Tint;
IMPORT MagPoint;

REVEAL
  FrameBuffer.T <: Output;

TYPE
  Output = FrameBuffer.Public OBJECT
  METHODS
  END;

PROCEDURE GetTint(frameBuffer : FrameBuffer.T; 
                  READONLY pt : MagPoint.T) : Tint.T;

END FrameBufferOutput.

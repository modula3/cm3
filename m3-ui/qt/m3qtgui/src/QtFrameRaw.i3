(*******************************************************************************
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.10
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
*******************************************************************************)

INTERFACE QtFrameRaw;


IMPORT Ctypes AS C;




<* EXTERNAL New_QFrame0 *>
PROCEDURE New_QFrame0 (parent: ADDRESS; f: C.int; ): QFrame;

<* EXTERNAL New_QFrame1 *>
PROCEDURE New_QFrame1 (parent: ADDRESS; ): QFrame;

<* EXTERNAL New_QFrame2 *>
PROCEDURE New_QFrame2 (): QFrame;

<* EXTERNAL Delete_QFrame *>
PROCEDURE Delete_QFrame (self: QFrame; );

<* EXTERNAL QFrame_frameStyle *>
PROCEDURE QFrame_frameStyle (self: QFrame; ): C.int;

<* EXTERNAL QFrame_setFrameStyle *>
PROCEDURE QFrame_setFrameStyle (self: QFrame; arg2: C.int; );

<* EXTERNAL QFrame_frameWidth *>
PROCEDURE QFrame_frameWidth (self: QFrame; ): C.int;

<* EXTERNAL QFrame_sizeHint *>
PROCEDURE QFrame_sizeHint (self: QFrame; ): ADDRESS;

<* EXTERNAL QFrame_frameShape *>
PROCEDURE QFrame_frameShape (self: QFrame; ): C.int;

<* EXTERNAL QFrame_setFrameShape *>
PROCEDURE QFrame_setFrameShape (self: QFrame; arg2: C.int; );

<* EXTERNAL QFrame_frameShadow *>
PROCEDURE QFrame_frameShadow (self: QFrame; ): C.int;

<* EXTERNAL QFrame_setFrameShadow *>
PROCEDURE QFrame_setFrameShadow (self: QFrame; arg2: C.int; );

<* EXTERNAL QFrame_lineWidth *>
PROCEDURE QFrame_lineWidth (self: QFrame; ): C.int;

<* EXTERNAL QFrame_setLineWidth *>
PROCEDURE QFrame_setLineWidth (self: QFrame; arg2: C.int; );

<* EXTERNAL QFrame_midLineWidth *>
PROCEDURE QFrame_midLineWidth (self: QFrame; ): C.int;

<* EXTERNAL QFrame_setMidLineWidth *>
PROCEDURE QFrame_setMidLineWidth (self: QFrame; arg2: C.int; );

<* EXTERNAL QFrame_frameRect *>
PROCEDURE QFrame_frameRect (self: QFrame; ): ADDRESS;

<* EXTERNAL QFrame_setFrameRect *>
PROCEDURE QFrame_setFrameRect (self: QFrame; arg2: ADDRESS; );

TYPE QFrame = ADDRESS;

END QtFrameRaw.

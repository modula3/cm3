(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:34:14 PST 1994 by heydon                   *)
(*      modified on Sun Jun  5 13:04:30 1994 by gnelson                      *)
(*      modified on Sat Aug 22 22:10:45 PDT 1992 by myers                    *)

INTERFACE JunoRT;

(* Defines the abstract Juno machine and the Juno run-time. See the
   "JunoByteCode" interface for a description of the instructions understood
   by the Juno machine.

   The Juno machine maintains an internal Boolean variable that is set by some
   instructions and tested by others. It also maintains a run-time stack,
   which contains both procedure frames and Juno values.

   All procedures in this interface are un-monitored. *)

IMPORT JunoRTError, JunoValue;
IMPORT Atom;

TYPE
  ByteCode = BITS 8 FOR [0..255];
  ByteStream = REF ARRAY OF ByteCode;

  ExternalCode = BRANDED "JunoRT.ExternalCode" OBJECT METHODS
    invoke(): BOOLEAN
  END;

(* If "ec" is an "ExternalCode" in the external code table, then "ec.invoke()"
   is called whenever "ec" is invoked by a "JunoByteCode.CALLEXT" bytecode.
   The machine's internal "cond" state variable is set to the return value
   produced by "ec.invoke()". The result should be "TRUE" in the event of
   success, and "FALSE" in the event of a run-time error. *)

VAR
  value_tbl: REF ARRAY OF JunoValue.T;
  code_tbl:  REF ARRAY OF ByteStream;
  ext_code_tbl: REF ARRAY OF ExternalCode;

(* The Juno machine has three global tables: one for values, one for compiled
   Juno procedures, and one for ``external'' Modula-3 procedures. Bytecode
   instructions can read and write values in the table "value_tbl", they can
   call "Code"'s in the table "code_tbl", and they can call "ExternalCode"'s
   (Modula-3 procedures) in the table "ext_code_tbl". The entry "code_tbl[0]"
   is reserved by the run-time.

   Use only the three procedures below to manipulate the value and code
   tables. *)

PROCEDURE GetValueIndex(val: JunoValue.T): CARDINAL;
(* Return the value index previously allocated to "val", or allocate and
   return a value index "i" for "val" (initializing "value_tbl[i]" to "val")
   if one has not been allocated for it previously. Indices returned by this
   procedure reference read-only values. *)

PROCEDURE GetVarIndex(md, nm: Atom.T): CARDINAL;
(* Return the value index previously allocated to the qualified ID "md . nm",
   or allocate and return a value index for that QID if one has not been
   allocated for it previously. *)

TYPE 
  ProcAttr = RECORD modName, name: Atom.T; sig: Sig END;
  Sig = RECORD outs, inouts, ins: CARDINAL END;

(* If "p: ProcAttr", "p.name" must be non-NIL, but "p.modName" may be
   NIL to indicate an anonymous module. *)
   
PROCEDURE GetCodeIndex(READONLY pa: ProcAttr): CARDINAL;
(* Return the code index previously allocated to the internal procedure
   with the name in "pa", or allocate and return an index 
   for that procedure if one has not been allocated for it previously. 
   Requires that "pa" be valid. *)

PROCEDURE GetExtCodeIndex(READONLY pa: ProcAttr): CARDINAL;
(* Return the code index previously allocated to the external procedure
   with the name in "pa", or allocate and return an index 
   for that procedure if one has not been allocated for it previously. 
   Requires that "pa" be valid. *)

PROCEDURE GetProcAttr(s: CARDINAL): ProcAttr;
(* Return the attributes of the internal procedure in slot "s". *)

PROCEDURE GetExtProcAttr(s: CARDINAL): ProcAttr;
(* Return the attributes of the external procedure in slot "s". *)

TYPE
  PC = RECORD proc: CARDINAL; offset: CARDINAL END;

(* If "pc: PC", then "pc" represents the program counter at the ByteCode
   "code_tbl[pc.proc, pc.offset]". *)

  Frame <: FramePublic;
  FramePublic = OBJECT METHODS
    down(): Frame;			 (* was "prev" *)
    up(): Frame;			 (* was "next" *)
    getLocal(i: INTEGER): JunoValue.T;
    setLocal(i: INTEGER; v: JunoValue.T);
    pc(): PC;
    setPC(pc: PC);
  END;

(* The Juno machine maintains a stack of frames. Each frame corresponds to a
   bytestream; no frames are created when procedures in the "ext_code_tbl" are
   called. A frame has two important attributes: an array of variables, and a
   current PC.

   The stack grows from the initial base frame upwards. Hence, the frame
   for the currently running procedure is the topmost frame. In the machine's
   initial state, the base and topmost frames are the same.

   The "down" and "up" methods report the adjoining "Frame"'s in the stack.
   The base frame always has "f.pc() = PC{0,0}" and "f.down() = NIL".  The
   topmost frame always has "f.up() = NIL".

   Local arguments in a frame (both procedure arguments and temporaries) can
   be accessed with the "getLocal" and "setLocal" methods. See the description
   in the "JunoByteCode" interface for how local variables are indexed. It is
   an unchecked run-time error to supply an index which is outside the valid
   range for the procedure to which the frame corresponds. The value returned
   by "getLocal" for unitialized variables may be (Modula-3) "NIL".

   The "pc" method reports the frame's program counter. The "setPC" method
   sets the frame's program counter.

   Various procedures cause "Frame" objects to become invalid. Once a frame
   becomes invalid, invoking any operation on it is an unchecked run-time
   error. *)

PROCEDURE BaseFrame(): Frame;
(* Returns the initial base frame. *)

PROCEDURE TopFrame(): Frame;
(* Returns the topmost frame. *)

PROCEDURE PushFrame(new_pc: PC; size: CARDINAL);
(* Creates a new frame "f" with "size" arguments such that "f.down()"
   is the current topmost frame, and "f.pc() = pc". Initially,
   "f.getArgument(i) = NIL" for each "i" in the range "[0..size-1]". If
   necessary, this procedure automatically enlarges the stack to hold the
   "size" arguments and the new frame pointer. This procedure sets "pc" to
   "new_pc". *)

PROCEDURE PopFrame();
(* Invalidates the current topmost frame; the frame below the the old topmost
   frame becomes the topmost frame. It is a checked run-time error to
   invoke this procedure when the machine is in the initial state. *)

PROCEDURE ResetMachine();
(* Resets the Juno machine to its initial state, in which the base and top
   frames are the same. *)

PROCEDURE Save();
(* Saves the current machine state, including the entire stack. Doesn't
   currently save the value table, though maybe it should. *)

PROCEDURE Restore();
(* Restores the state to the one that was last saved by a call to Save(). It
   is a checked run-time error if no state has been previously saved. *)

PROCEDURE GetStackSize(): CARDINAL;
(* Reports the current size of the run-time stack. *)

PROCEDURE EnlargeStack();
(* Doubles the size of the run-time stack, while preserving all aspects of
   the machine state. *)

TYPE TrapCode = {NormalHalt, BreakPoint, Error, Interrupt, StackOverflow};

(* A "NormalHalt" trap occurs when a procedure returns into a frame "f" such
   that "f.pc() = PC{0,0}". An "Error" trap occurs in the event of a run-time
   error (see the "JunoRTError" interface for a list of possible run-time
   errors). An "Interrupt" trap occurs when the computation is interrupted by
   the user. A "StackOverflow" trap occurs when the run-time stack is
   overflowed; calling "EnlargeStack" followed by "Exec" will continue the
   computation. *)

TYPE ExecRes = RECORD
  trapCode: TrapCode;
  trapLoc: PC;
  extSlot: CARDINAL;
  errorCode: JunoRTError.Code
END;

(* The procedures in the module for running the machine return a result of
   type "ExecRes". If "er: ExecRes", then "er.trapCode" is the "TrapCode"
   indicating why the machine stopped. If "trapCode # NormalHalt", then
   "er.trapLoc" is the program counter of the instruction causing a run-time
   error if "trapCode = Error", or the location of the most recently executed
   instruction otherwise. The field "er.extSlot" is the slot number of the most
   recently executed external procedure; it is only defined in the event that
   the machine halted due to the failure of an external procedure. If
   "er.trapCode = TrapCode.Error", "er.errorCode" is the error code value. *)

PROCEDURE TrapMessage(READONLY execRes: ExecRes): TEXT;
(* Return a descriptive error message for the execution result "execRes". *)

PROCEDURE Interrupt();
(* Halt the currently running program at a convenient location with a
   "TrapCode.Interrupt", and then return. This is a no-op if the machine
   is not currently running. *)

PROCEDURE Exec(): ExecRes;
(* Executes any code at the current "pc" of the bottom stack frame until a
   trap is encountered, and returns the execution result. All existing
   "Frame" objects become invalid once this procedure is invoked. *)

PROCEDURE ExecFromSlot(slot: CARDINAL; reset := TRUE): ExecRes;
(* Execute the code in the given "code_tbl" slot. If "reset = TRUE", then call
   "ResetMachine" before executing the specified code. *)

END JunoRT.

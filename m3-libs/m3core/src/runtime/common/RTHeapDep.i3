(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Sat Nov 19 09:23:17 PST 1994 by kalsow  *)
(*      modified on Thu Dec 24 15:36:20 PST 1992 by jdd     *)

INTERFACE RTHeapDep;

IMPORT Word, RTMachine;

(* This is the interface to the machine-dependent portion
   of the traced heap allocator and collector. *)

(* The heap page size is machine-dependent, since it might depend on the
   architecture's VM page size (if VM is TRUE).  Otherwise, 8192 bytes is a
   reasonable page size.  The page size must be a power of two. *)

CONST
  BytesPerPage    = RTMachine.BytesPerHeapPage;    (* bytes per page *)
  LogBytesPerPage = RTMachine.LogBytesPerHeapPage;
  AdrPerPage      = RTMachine.AdrPerHeapPage;      (* addresses per page *)
  LogAdrPerPage   = RTMachine.LogAdrPerHeapPage;

TYPE Page = [0 .. Word.Divide(-1, AdrPerPage)];

(* The collector supports the use of VM protection to achieve incremental,
   generational collection.  This is not possible on all architectures, and
   it may not be implemented in all cases where it is possible.  The
   boolean constant VM is TRUE iff all necessary support is present for
   this architecture.  VM is TRUE for the DS3100, whose implementation you
   might use as a reference. *)

CONST VM = RTMachine.VMHeap;

(* None of the procedures below will be called if VM is FALSE.  The
   descriptions given for each assume that VM is TRUE. *)

(* If VM is TRUE, there must also be machine-dependent code to catch
   all protection faults that might reference the heap, and call
   RTHeap.Fault.  In addition to ordinary instructions that may reference
   heap pages while they are protected, this also includes system calls
   that might fault when reading or writing heap locations. *)

PROCEDURE Protect (p: Page; n: CARDINAL; readable, writable: BOOLEAN);

(* When pages are allocated from the heap, they are assumed to
   readable and writable, although not executable.  Protect is used to
   change memory protection on heap pages.

   Protect changes the protection of pages [p..p+n-1] as specified by
   "readable" and "writable".  The page remains unexecutable.  If readable
   is FALSE and writable is TRUE, but the architecture does not support
   this combination, it is acceptable for Protect to make the page neither
   readable nor writable.

   Note that the "pages" are heap pages, not VM pages; the locations
   protected are p*BytesPerPage through (p+n)*BytesPerPage-1.  If VM is
   TRUE, BytesPerPage must be at least the VM page size. *)

PROCEDURE TimeUsed (): REAL;

(* TimeUsed returns the amount of processor time used by the program
   so far, in arbitrary units.  This information is used to tune the
   generational collector. *)

PROCEDURE VMFaultTime (): REAL;

(* VMFaultTime returns the amount of processor time required to handle
   a VM fault, in the same units as TimeUsed.  This information is used
   to tune the generational collector. *)

END RTHeapDep.

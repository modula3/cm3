INTERFACE RTVM;

(*
  The procedures of this interface used to be defined as constants in
  the target specific interface RTMachine.i3. They were moved here in
  order to make it possible to extract the complex and
  difficult-to-port parts of the m3 runtime concerned with vm
  protection and system call wrappers into their own library,
  m3gc-enhanced. A simple support library, m3gc-simple, which does not
  enable incremental and generational garbage collection, has also
  been added.
*)

<*EXTERNAL*> PROCEDURE VMHeap() : BOOLEAN;
(* The collector supports the use of VM protection to achieve incremental,
   generational collection.  This is not possible on all architectures, and
   it may not be implemented in all cases where it is possible.  This
   procedure must return "TRUE" iff all necessary support is
   present for this architecture. *)

<*EXTERNAL*> PROCEDURE AtomicWrappers() : BOOLEAN;
(* If "VMHeap" is true, "AtomicWrappers" indicates whether the wrappers
   that validate parameters passed to system calls are atomic with
   respect to the collector.  *)

END RTVM.

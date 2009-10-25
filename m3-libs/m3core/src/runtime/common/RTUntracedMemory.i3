(*
This is a thin layer over malloc/calloc/free.
It exists strictly so that other allocators might be used instead,
such as on Windows going directly to HeapAlloc(GetProcessHeap()) and
reducing C runtime dependency.
*)

UNSAFE INTERFACE RTUntracedMemory;

<*EXTERNAL RTUntracedMemory__AllocZ*>
PROCEDURE AllocZ(count: INTEGER): ADDRESS;
(* Z for zeroed (calloc) *)

<*EXTERNAL RTUntracedMemory__AllocZV*>
PROCEDURE AllocZV(count, size: INTEGER): ADDRESS;
(* ZV for zeroed vector (calloc) *)

<*EXTERNAL RTUntracedMemory__Free*>
PROCEDURE Free(VAR a: ADDRESS);
(* a will be NIL when done *)

END RTUntracedMemory.

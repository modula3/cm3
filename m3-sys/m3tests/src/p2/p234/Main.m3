MODULE Main;

(* This program endeavors to use or not use volatile registers,
 * in order to get the backend to save/restore them or not.
 *
 * This probably belongs in the "c for code" section, though
 * we can run it and make sure it doesn't crash.
 *)

PROCEDURE no_nonvolatile_registers() = BEGIN
END no_nonvolatile_registers;

PROCEDURE ebx(a,b:INTEGER)=
(* It is stupid that this uses EBX, but oh well.
 * It should favor volatile registers such as EAX, ECX, EDX.
 *)
BEGIN
  a := b;
END ebx;

VAR g:ARRAY[0..100] OF INTEGER;
VAR h:ARRAY[0..100] OF INTEGER;

PROCEDURE esi_edi()=
(* This uses ESI and EDI as part of a block copy. *)
BEGIN
  g := h;
END esi_edi;

PROCEDURE esi_edi_ebx(a,b:INTEGER)=
(* This uses all three non-volatile registers as a simple
 * combination of esi_edi and ebx.
 *)
BEGIN
  a := b;
  g := h;
END esi_edi_ebx;

BEGIN
  no_nonvolatile_registers();
  ebx(1,2);
  esi_edi();
  esi_edi_ebx(1,2);
END Main.
